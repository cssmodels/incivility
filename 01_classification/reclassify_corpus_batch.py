"""
Classifies political incivility (0-3 ordinal scale) for the full tweet corpus
using the OpenAI Batch API (gpt-4.1, codebook prompt validated against human
coders at kappa_pop = 0.85).

Retweets are excluded -- the text is truncated and the author is not the
politician, so scoring them is not meaningful. Only original tweets
(retweet_id IS NULL) are classified. Uses text_norm (URLs replaced with
<URL>) rather than raw text.

The script can be run repeatedly: it submits any unsubmitted chunks, then
polls in-flight batches and downloads completed ones. Already-completed
chunks and already-submitted batches are skipped automatically.

    export OPENAI_API_KEY=sk-...
    python3.12 reclassify_corpus_batch.py            # submit + poll
    python3.12 reclassify_corpus_batch.py --test      # first 200 rows only

At batch size 50, ~15.4M original tweets produce ~308k requests. Batch
turnaround is up to 24h per file under OpenAI's SLA.
"""

from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path

import pandas as pd
from openai import OpenAI
from tqdm import tqdm

# Paths
CHUNKS_DIR = Path("/Volumes/CARDSPACE/2026toxic/incivility_unique_run/incivility_final")
OUTPUT_DIR = Path("/Volumes/CARDSPACE/2026toxic/incivility_v2")
BATCH_DIR = Path("/Volumes/CARDSPACE/2026toxic/batch_jobs")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
BATCH_DIR.mkdir(parents=True, exist_ok=True)

STATE_FILE = BATCH_DIR / "batch_state.json"  # {chunk_name: {batch_id, status}}

MODEL = "gpt-4.1"

PROMPT = """\
You are coding political incivility in tweets by politicians. Code only the author's communicative posture, not whether you agree with the claim or whether the claim is true.

Assign one score per tweet: 0, 1, 2, or 3.

SCALE

0 = Civil
No incivility. The tweet may criticize policies, decisions, events, parties, or performance, but it does not use insults, contemptuous language, profanity, derogatory labels, or delegitimizing attacks.
Examples: policy criticism, factual claims, announcements, disagreement, condolences, praise, campaign updates.

1 = Mildly uncivil
Dismissive, sarcastic, mocking, condescending, or mildly contemptuous language, but no direct strong attack. Also code 1 for profanity used only as emphasis rather than as a targeted insult.
Examples: "predictably," "what a surprise," "they should be ashamed," "this is crap," "another brilliant idea from…"

2 = Very uncivil
Direct attack on a person, party, institution, or group. Includes explicit insults, derogatory labels, accusations of lying, corruption, betrayal, malice, incompetence, criminality, hypocrisy, or deliberate deception; targeted profanity; and delegitimizing opponents as enemies, traitors, criminals, racists, extremists, etc.
Examples: "liar," "corrupt," "traitor," "racist," "crooks," "goons," "idiots," "they are destroying the country," "this party is a criminal organization."

3 = Extremely uncivil
Severe norm violation: dehumanization, explicit hate speech, threats, intimidation, calls for violence, endorsement/celebration of harm, wishes of death/destruction, or categorical exclusion of groups from society/politics.
Examples: "parasites," "vermin," "they should be killed," "wipe them out," "nuke them," "they have no place in our society," explicit racist/religious hate, calls for expulsion of an entire group.

DECISION RULES

1. Code the highest level of incivility present. One strongly uncivil phrase is enough.
2. Strong criticism is not automatically uncivil. "The policy failed" = 0. "The minister is incompetent" = 2.
3. Profanity always raises the score: non-targeted profanity = at least 1; targeted profanity/abuse = at least 2.
4. Sarcasm/mockery without direct attack = usually 1. Sarcasm used to make a direct attack = 2.
5. Attacks on institutions, parties, media, courts, governments, or social groups count the same as attacks on individuals.
6. Derogatory language toward ethnic, religious, gender, sexual, immigrant, or other social groups is at least 2; hate speech, dehumanization, or calls for exclusion/violence are 3.
7. If the tweet quotes or reports uncivil language by others in order to condemn it, code the author's posture, not the quoted content. Usually this is 0, but note that graphic quoted abuse was present.
8. If the tweet reports threats or violence without endorsing them, code the reporting posture, usually 0.
9. If the tweet is unclear, only a link, only a hashtag with no clear incivility, or impossible to evaluate, code 0.
10. When genuinely unsure between two scores, choose the lower score.

Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)"""

BATCH_SIZE = 50  # tweets per API call (one JSONL line)
MAX_REQUESTS = 45_000  # OpenAI's per-batch-file limit is 50k


def load_state() -> dict:
    if STATE_FILE.exists():
        return json.loads(STATE_FILE.read_text())
    return {}


def save_state(state: dict) -> None:
    STATE_FILE.write_text(json.dumps(state, indent=2))


def build_request(custom_id: str, tweets: list[tuple[str, str]]) -> dict:
    user_content = "id\ttext\n" + "\n".join(
        f"{tid}\t{txt[:500]}" for tid, txt in tweets
    )
    return {
        "custom_id": custom_id,
        "method": "POST",
        "url": "/v1/chat/completions",
        "body": {
            "model": MODEL,
            "temperature": 0,
            "response_format": {"type": "json_object"},
            "messages": [
                {"role": "system", "content": PROMPT},
                {"role": "user", "content": user_content},
            ],
        },
    }


def write_jsonl(path: Path, requests: list[dict]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        for req in requests:
            f.write(json.dumps(req, ensure_ascii=False) + "\n")


def prepare_chunk(chunk_path: Path, test: bool = False) -> list[dict]:
    """Build batch request dicts for one chunk, dropping retweets."""
    df = pd.read_parquet(chunk_path)
    n_total = len(df)
    df = df[df["retweet_id"].isna()].copy()
    n_orig = len(df)
    if test:
        df = df.head(200)
    print(f"    {n_orig:,} original tweets ({n_total - n_orig:,} retweets dropped)")
    rows = list(zip(df["id"].astype(str), df["text_norm"].fillna("")))
    requests = []
    for i in range(0, len(rows), BATCH_SIZE):
        batch = rows[i : i + BATCH_SIZE]
        cid = f"{chunk_path.stem}__b{i // BATCH_SIZE:05d}"
        requests.append(build_request(cid, batch))
    return requests


def submit_batch(client: OpenAI, jsonl_path: Path) -> str:
    with open(jsonl_path, "rb") as f:
        file_obj = client.files.create(file=f, purpose="batch")
    batch = client.batches.create(
        input_file_id=file_obj.id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
    )
    return batch.id


def process_results(client: OpenAI, batch_id: str,
                    chunk_path: Path, out_path: Path,
                    test: bool = False) -> None:
    """Download a completed batch, decode scores, write parquet."""
    batch = client.batches.retrieve(batch_id)
    output_file_id = batch.output_file_id
    if not output_file_id:
        print(f"  WARNING: no output file for batch {batch_id} (chunk {chunk_path.name})")
        return

    content = client.files.content(output_file_id).text
    results: dict[str, int] = {}
    for line in content.splitlines():
        if not line.strip():
            continue
        item = json.loads(line)
        try:
            body = item["response"]["body"]
            raw = json.loads(body["choices"][0]["message"]["content"])
            for tid, score in raw.get("labels", []):
                results[str(tid)] = int(score)
        except Exception:
            pass  # malformed line -- tweet stays unscored

    df = pd.read_parquet(chunk_path)
    if test:
        df = df.head(200)
    df["incivility_llm_v2"] = df["id"].astype(str).map(results)
    missing = df["incivility_llm_v2"].isna().sum()
    if missing > 0:
        print(f"  WARNING: {missing} tweets unscored in {chunk_path.name}")

    df.to_parquet(out_path, index=False, compression="snappy")
    scored = df["incivility_llm_v2"].notna().sum()
    dist = df["incivility_llm_v2"].value_counts(normalize=True).sort_index()
    dist_str = "  ".join(f"{int(k)}:{v:.1%}" for k, v in dist.items())
    print(f"  DONE {chunk_path.name}: {scored}/{len(df)} scored  [{dist_str}]")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--test", action="store_true",
                        help="Process only first 200 rows of first chunk")
    args = parser.parse_args()

    client = OpenAI()
    state = load_state()

    chunk_paths = sorted(CHUNKS_DIR.glob("m_slim_*.parquet"))
    print(f"Found {len(chunk_paths)} chunks in {CHUNKS_DIR}")
    if args.test:
        chunk_paths = chunk_paths[:1]
        print("TEST MODE: first chunk only (200 rows)")

    # Submit any chunks that aren't already in flight or done
    newly_submitted = 0
    for chunk_path in chunk_paths:
        name = chunk_path.stem
        out_path = OUTPUT_DIR / chunk_path.name

        if out_path.exists() and out_path.stat().st_size > 1_000_000 and not args.test:
            print(f"  SKIP {chunk_path.name} (output already written)")
            continue
        if name in state and state[name]["status"] in ("submitted", "completed"):
            continue

        print(f"  Preparing {chunk_path.name}…", end=" ", flush=True)
        requests = prepare_chunk(chunk_path, test=args.test)

        file_batches = [
            requests[i : i + MAX_REQUESTS]
            for i in range(0, len(requests), MAX_REQUESTS)
        ]
        batch_ids = []
        for fi, fb in enumerate(file_batches):
            jsonl_path = BATCH_DIR / f"{name}_part{fi:02d}.jsonl"
            write_jsonl(jsonl_path, fb)
            bid = submit_batch(client, jsonl_path)
            batch_ids.append(bid)
            print(f"submitted batch {bid} ({len(fb)} reqs)", end=" ", flush=True)

        state[name] = {"batch_ids": batch_ids, "status": "submitted",
                       "chunk_path": str(chunk_path)}
        save_state(state)
        print()
        newly_submitted += 1

    if newly_submitted:
        print(f"\nSubmitted {newly_submitted} new chunks. "
              "Run again in a few hours to poll and download results.")

    # Poll pending batches, download anything that's finished
    print("\nPolling pending batches…")
    pending = {n: v for n, v in state.items() if v["status"] == "submitted"}
    if not pending:
        print("  Nothing pending.")
    else:
        print(f"  {len(pending)} chunk(s) in flight.")

    for name, entry in pending.items():
        chunk_path = Path(entry["chunk_path"])
        out_path = OUTPUT_DIR / chunk_path.name
        batch_ids = entry["batch_ids"]

        statuses = []
        for bid in batch_ids:
            b = client.batches.retrieve(bid)
            statuses.append(b.status)

        print(f"  {chunk_path.name}: {statuses}")

        if all(s == "completed" for s in statuses):
            all_results: dict[str, int] = {}
            for bid in batch_ids:
                b = client.batches.retrieve(bid)
                content = client.files.content(b.output_file_id).text
                for line in content.splitlines():
                    if not line.strip():
                        continue
                    item = json.loads(line)
                    try:
                        body = item["response"]["body"]
                        raw = json.loads(body["choices"][0]["message"]["content"])
                        for tid, score in raw.get("labels", []):
                            all_results[str(tid)] = int(score)
                    except Exception:
                        pass

            df = pd.read_parquet(chunk_path)
            df = df[df["retweet_id"].isna()].copy()
            if args.test:
                df = df.head(200)
            df["incivility_llm_v2"] = df["id"].astype(str).map(all_results)
            missing = df["incivility_llm_v2"].isna().sum()
            if missing:
                print(f"    WARNING: {missing} unscored tweets")
            df.to_parquet(out_path, index=False, compression="snappy")
            scored = df["incivility_llm_v2"].notna().sum()
            dist = df["incivility_llm_v2"].value_counts(normalize=True).sort_index()
            dist_str = "  ".join(f"{int(k)}:{v:.1%}" for k, v in dist.items())
            print(f"    WRITTEN {chunk_path.name}: {scored}/{len(df)} scored  [{dist_str}]")
            state[name]["status"] = "completed"
            save_state(state)

        elif any(s in ("failed", "expired", "cancelled") for s in statuses):
            print(f"    PROBLEM: {name} batch(es) {statuses} — resubmit manually")
            state[name]["status"] = "failed"
            save_state(state)

    done = sum(1 for v in state.values() if v["status"] == "completed")
    infly = sum(1 for v in state.values() if v["status"] == "submitted")
    print(f"\nStatus: {done} chunks done, {infly} in flight, "
          f"{len(chunk_paths) - done - infly} not yet submitted")

    if done == len(chunk_paths):
        print("\nCorpus summary:")
        totals: dict = {0: 0, 1: 0, 2: 0, 3: 0, "n": 0}
        for p in sorted(OUTPUT_DIR.glob("m_slim_*.parquet")):
            try:
                df = pd.read_parquet(p, columns=["incivility_llm_v2"])
                for s in [0, 1, 2, 3]:
                    totals[s] += (df["incivility_llm_v2"] == s).sum()
                totals["n"] += len(df)
            except Exception:
                pass
        n = totals["n"]
        if n > 0:
            print(f"  Total tweets: {n:,}")
            for s in [0, 1, 2, 3]:
                print(f"  Score {s}: {totals[s]:,}  ({totals[s]/n:.1%})")


if __name__ == "__main__":
    main()
