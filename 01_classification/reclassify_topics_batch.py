"""
Classifies tweets into one of 9 topic categories using the OpenAI Batch API
(gpt-4.1-mini, validated against human gold-standard labels at kappa = 0.80).

Three of the categories merge pairs from an earlier 12-category scheme:
Economy (Economic + Welfare), Cultural Politics (Identity + Nationalism),
and Law & Order (Crime + Security). The other six categories are unchanged.

Input: incivility_v2 chunks (retweets already excluded there).
Output: topics_v2 chunks with an added topic_v2 column.

Like reclassify_corpus_batch.py, this submits unsubmitted chunks and then
polls/downloads in-flight batches, and can be run repeatedly:

    export OPENAI_API_KEY=sk-...
    python3.12 reclassify_topics_batch.py
    python3.12 reclassify_topics_batch.py --test

Batch size is 20 here rather than 50 (topic classification needs more
context per tweet than incivility scoring), giving ~770k requests for the
~15.4M-tweet corpus.
"""

from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path

import pandas as pd
from openai import OpenAI

# Paths
CHUNKS_DIR = Path("/Volumes/CARDSPACE/2026toxic/incivility_v2")
OUTPUT_DIR = Path("/Volumes/CARDSPACE/2026toxic/topics_v2")
BATCH_DIR = Path("/Volumes/CARDSPACE/2026toxic/batch_jobs_topics")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
BATCH_DIR.mkdir(parents=True, exist_ok=True)

STATE_FILE = BATCH_DIR / "batch_state.json"

MODEL = "gpt-4.1-mini"
BATCH_SIZE = 20
MAX_REQUESTS = 17_000  # prompt is ~10.5KB/req, so 17k reqs keeps files under the 200MB limit

PROMPT = """\
You are classifying tweets by politicians into exactly one topic category (any language).

CATEGORIES
  Cultural Politics – debates about social identity, values, and the nation's relationship
                      to outside entities. Covers TWO areas:
                      (a) IDENTITY: race, gender, LGBTQ+, religion, ethnicity, culture wars,
                      abortion, historical memory, social values — debates about people's
                      characteristics or identity groups already present in society.
                      (b) NATIONALISM: sovereignty, national independence, Brexit, anti-EU
                      sentiment, national pride as political project, Euroscepticism focused
                      on sovereignty — the nation-state vs. outside entities.
                      IMPORTANT: Migration = policy about controlling immigration flows and
                      borders. Cultural Politics = debates about identity/values/sovereignty.
                      A tweet calling for stricter border controls = Migration. A tweet about
                      how immigrants undermine national culture or identity = Cultural Politics.
                      Expressions of national pride, flag-waving, "our country first" rhetoric,
                      and sovereignty arguments = Cultural Politics, NOT Other.
  Democracy         – democratic processes, institutions, electoral politics, parliamentary
                      procedure, party politics, democratic norms, election campaigns, voting
                      rights, media freedom, comments on other parties or politicians, political
                      scandals, accusations of corruption or criminality against politicians
                      (= democratic accountability, NOT Law & Order). IMPORTANT: campaign
                      slogans, calls to vote, endorsements of parties/politicians, expressions
                      of support or opposition toward political opponents, and any tweet
                      primarily directed at a political actor = Democracy, NOT Other.
  Economy           – ALL economic and social spending policy. Covers TWO areas:
                      (a) ECONOMIC: aggregate economy — taxation, fiscal policy, trade, business
                      regulation, inflation, wages, GDP, employment levels, budget.
                      (b) WELFARE: individual services and benefits — healthcare, social insurance,
                      housing, disability benefits, family policy, childcare, pensions.
                      Use Economy for both aggregate economic policy AND individual welfare services.
  Education         – education policy, schools, universities, curricula, student debt,
                      educational inequality.
  Environment       – environmental protection, climate change, energy policy, renewable
                      energy, pollution, biodiversity. IMPORTANT: if a tweet addresses both
                      environmental/climate concerns AND economic impacts (e.g. green jobs,
                      energy costs), classify as Environment if climate or environmental
                      protection is the primary frame.
  Law & Order       – crime, policing, and national security. Covers TWO areas:
                      (a) CRIME: crime policy, law enforcement, criminal justice, sentencing,
                      prisons, drugs (enforcement context), law-and-order.
                      (b) SECURITY: national security, defence, military, terrorism, policing in
                      a security context, intelligence, international conflicts.
                      Use Law & Order for both domestic crime policy AND national/military security.
                      IMPORTANT: accusations that a politician is corrupt/criminal = Democracy
                      (democratic accountability), NOT Law & Order. Tiebreaker: if the primary
                      subject is a security threat, crime wave, military operation, or policing
                      policy = Law & Order. If the primary subject is what a politician or party
                      did or should do = Democracy.
  Migration         – immigration, asylum, refugees, border control, integration, deportations.
                      Migration = policy about immigration flows, border controls, and asylum
                      processes. Do NOT use for tweets about cultural identity or sovereignty
                      (those = Cultural Politics).
  Other             – genuinely non-political or non-assignable: greetings, congratulations,
                      condolences, sports scores, entertainment, personal life announcements,
                      or tweets that are literally impossible to interpret (single emoji, only
                      a URL, pure gibberish). ONLY use Other when no political or policy content
                      whatsoever can be identified. Do NOT use Other for: campaign slogans,
                      calls to vote, endorsements, attacks on opponents, expressions of national
                      pride, policy criticism (however brief), or any tweet where the author's
                      political stance is identifiable.
  Technology        – technology, digital policy, social media regulation, AI, cybersecurity,
                      media/press policy (except press freedom, which is Democracy).

KEY DECISION RULES
1. Economy covers BOTH aggregate economic policy (taxes, GDP, trade) AND individual
   welfare/social services (healthcare, housing, childcare, pensions). Use Economy for either.
2. Cultural Politics covers BOTH identity debates (race, gender, religion, sexuality,
   abortion, culture wars) AND nationalist/sovereignty debates (Brexit, anti-EU, national
   independence). Use Cultural Politics for either.
3. Migration = immigration flows/borders/asylum. Cultural Politics = identity/values/sovereignty.
   They are distinct: border policy = Migration; cultural threat framing = Cultural Politics.
4. Law & Order covers BOTH domestic crime policy AND national security/military/defence.
   Use Law & Order for either.
5. Accusations of political corruption or criminality = Democracy (democratic accountability).
6. Environment takes priority over Economy when climate/environment is the primary frame,
   even if economic effects are also mentioned.
7. Law & Order vs Democracy tiebreaker: primary subject is a threat/crime/military operation
   = Law & Order; primary subject is what a politician or party did or should do = Democracy.
8. Other: ONLY for personal/entertainment/congratulatory content with zero political substance.
   Campaign slogans, political attacks, national pride, any policy stance = NOT Other.
9. Cultural Politics includes brief expressions of national pride or "our country first"
   rhetoric — do not code these as Other.

Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id","Category"],...]} (every id once, Category from list above)"""

VALID_CATEGORIES = {
    "Cultural Politics", "Democracy", "Economy", "Education", "Environment",
    "Law & Order", "Migration", "Other", "Technology",
}


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
    """Build batch request dicts for one chunk."""
    df = pd.read_parquet(chunk_path)
    # incivility_v2 files already have retweets removed, but guard just in case
    if "retweet_id" in df.columns:
        df = df[df["retweet_id"].isna()].copy()
    if test:
        df = df.head(200)
    print(f"    {len(df):,} tweets")
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


def process_completed(client: OpenAI, entry: dict,
                      chunk_path: Path, out_path: Path,
                      test: bool = False) -> bool:
    """Download all batch parts for a chunk, merge results, write parquet."""
    all_results: dict[str, str] = {}
    for bid in entry["batch_ids"]:
        b = client.batches.retrieve(bid)
        if not b.output_file_id:
            print(f"  WARNING: no output file for batch {bid}")
            return False
        content = client.files.content(b.output_file_id).text
        for line in content.splitlines():
            if not line.strip():
                continue
            try:
                item = json.loads(line)
                body = item["response"]["body"]
                raw = json.loads(body["choices"][0]["message"]["content"])
                for tid, cat in raw.get("labels", []):
                    all_results[str(tid)] = str(cat)
            except Exception:
                pass

    df = pd.read_parquet(chunk_path)
    if "retweet_id" in df.columns:
        df = df[df["retweet_id"].isna()].copy()
    if test:
        df = df.head(200)

    df["topic_v2"] = df["id"].astype(str).map(all_results)
    df["topic_v2"] = df["topic_v2"].where(df["topic_v2"].isin(VALID_CATEGORIES), other=None)

    missing = df["topic_v2"].isna().sum()
    if missing > 0:
        print(f"  WARNING: {missing} tweets unclassified in {chunk_path.name}")

    df.to_parquet(out_path, index=False, compression="snappy")
    scored = df["topic_v2"].notna().sum()
    dist = df["topic_v2"].value_counts().head(5).to_dict()
    dist_str = "  ".join(f"{k}:{v:,}" for k, v in dist.items())
    print(f"  DONE {chunk_path.name}: {scored:,}/{len(df):,} classified  top5=[{dist_str}]")
    return True


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
            print(f"submitted {bid} ({len(fb):,} reqs)", end=" ", flush=True)

        state[name] = {"batch_ids": batch_ids, "status": "submitted",
                       "chunk_path": str(chunk_path)}
        save_state(state)
        print()
        newly_submitted += 1

    if newly_submitted:
        print(f"\nSubmitted {newly_submitted} new chunk(s). "
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

        statuses = [client.batches.retrieve(bid).status for bid in batch_ids]
        print(f"  {chunk_path.name}: {statuses}")

        if all(s == "completed" for s in statuses):
            ok = process_completed(client, entry, chunk_path, out_path, test=args.test)
            if ok:
                state[name]["status"] = "completed"
                save_state(state)
        elif any(s in ("failed", "expired", "cancelled") for s in statuses):
            print(f"  PROBLEM: {name} → {statuses} — resubmit manually")
            state[name]["status"] = "failed"
            save_state(state)

    done = sum(1 for v in state.values() if v["status"] == "completed")
    infly = sum(1 for v in state.values() if v["status"] == "submitted")
    print(f"\nStatus: {done}/{len(chunk_paths)} chunks done, "
          f"{infly} in flight, {len(chunk_paths) - done - infly} not yet submitted")

    if done == len(chunk_paths):
        print("\nCorpus summary:")
        totals: dict[str, int] = {}
        n_total = 0
        for p in sorted(OUTPUT_DIR.glob("m_slim_*.parquet")):
            try:
                df = pd.read_parquet(p, columns=["topic_v2"])
                for cat, cnt in df["topic_v2"].value_counts().items():
                    totals[cat] = totals.get(cat, 0) + int(cnt)
                n_total += len(df)
            except Exception:
                pass
        if n_total > 0:
            print(f"  Total tweets: {n_total:,}")
            for cat, cnt in sorted(totals.items(), key=lambda x: -x[1]):
                print(f"  {cat:<18}: {cnt:>10,}  ({cnt/n_total:.1%})")


if __name__ == "__main__":
    main()
