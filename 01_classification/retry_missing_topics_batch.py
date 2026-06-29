"""
Retries tweets left unclassified (topic_v2 is NaN) after a batch run of
reclassify_topics_batch.py -- batches occasionally have a handful of
malformed responses that fail to parse.

For each topics_v2 parquet, finds rows with topic_v2 still missing, submits
them to the Batch API, and merges results back into the parquet in place.
Same model, prompt, and batch settings as the original run.

    export OPENAI_API_KEY=sk-...
    python3.12 retry_missing_topics_batch.py   # submit, then poll on rerun

Run repeatedly until "Total unclassified tweets remaining" hits zero.
"""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd
from openai import OpenAI

# Paths
TOPICS_DIR = Path("/Volumes/CARDSPACE/2026toxic/topics_v2")
INCIV_DIR = Path("/Volumes/CARDSPACE/2026toxic/incivility_v2")  # fallback source
BATCH_DIR = Path("/Volumes/CARDSPACE/2026toxic/batch_jobs_topics")
BATCH_DIR.mkdir(parents=True, exist_ok=True)

STATE_FILE = BATCH_DIR / "batch_state_retry.json"

MODEL = "gpt-4.1-mini"
BATCH_SIZE = 20
MAX_REQUESTS = 17_000  # ~10.5KB/req, so 17k reqs keeps files under the 200MB limit

VALID_CATEGORIES = {
    "Cultural Politics", "Democracy", "Economy", "Education", "Environment",
    "Law & Order", "Migration", "Other", "Technology",
}

# Same prompt as reclassify_topics_batch.py
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


def submit_batch(client: OpenAI, jsonl_path: Path) -> str:
    with open(jsonl_path, "rb") as f:
        file_obj = client.files.create(file=f, purpose="batch")
    batch = client.batches.create(
        input_file_id=file_obj.id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
    )
    return batch.id


def main() -> None:
    client = OpenAI()
    state = load_state()

    # Source chunks are the ground truth for what should exist downstream
    all_chunk_names = sorted(p.name for p in INCIV_DIR.glob("m_slim_*.parquet"))
    print(f"Found {len(all_chunk_names)} source chunks; "
          f"{len(list(TOPICS_DIR.glob('m_slim_*.parquet')))} written so far")

    # Find chunks with missing tweets and submit retry batches
    newly_submitted = 0
    for chunk_name in all_chunk_names:
        chunk_path = TOPICS_DIR / chunk_name
        name = Path(chunk_name).stem + "_retry"

        if name in state and state[name]["status"] in ("submitted", "completed"):
            continue

        if chunk_path.exists():
            df = pd.read_parquet(chunk_path)
            missing_df = df[df["topic_v2"].isna()].copy()
        else:
            # Output parquet was never written (batch failed entirely) -- rebuild from source
            src_path = INCIV_DIR / chunk_name
            print(f"  {chunk_name}: no output parquet — reading source for full retry")
            df = pd.read_parquet(src_path)
            if "retweet_id" in df.columns:
                df = df[df["retweet_id"].isna()].copy()
            df["topic_v2"] = None
            missing_df = df.copy()

        n_missing = len(missing_df)
        if n_missing == 0:
            continue

        print(f"  {chunk_path.name}: {n_missing:,} missing tweets — submitting retry…")

        text_col = "text_norm" if "text_norm" in missing_df.columns else "text"
        rows = list(zip(
            missing_df["id"].astype(str),
            missing_df[text_col].fillna("")
        ))
        requests = []
        for i in range(0, len(rows), BATCH_SIZE):
            batch = rows[i : i + BATCH_SIZE]
            cid = f"{chunk_path.stem}_retry__b{i // BATCH_SIZE:05d}"
            requests.append(build_request(cid, batch))

        file_batches = [
            requests[i : i + MAX_REQUESTS]
            for i in range(0, len(requests), MAX_REQUESTS)
        ]
        batch_ids = []
        for fi, fb in enumerate(file_batches):
            jsonl_path = BATCH_DIR / f"{chunk_path.stem}_retry_part{fi:02d}.jsonl"
            write_jsonl(jsonl_path, fb)
            bid = submit_batch(client, jsonl_path)
            batch_ids.append(bid)
            print(f"    part{fi}: submitted {bid} ({len(fb):,} reqs)")

        state[name] = {
            "batch_ids": batch_ids,
            "status": "submitted",
            "chunk_path": str(chunk_path),
            "src_chunk_path": str(INCIV_DIR / chunk_name),
            "n_missing": n_missing,
        }
        save_state(state)
        newly_submitted += 1

    if newly_submitted:
        print(f"\nSubmitted retry batches for {newly_submitted} chunk(s). "
              "Run again in a few hours to poll and download results.")
    else:
        print("No new chunks need retry submission.")

    # Poll pending retry batches
    print("\nPolling pending retry batches…")
    pending = {n: v for n, v in state.items() if v["status"] == "submitted"}
    if not pending:
        print("  Nothing pending.")
    else:
        print(f"  {len(pending)} chunk(s) in flight.")

    for name, entry in pending.items():
        chunk_path = Path(entry["chunk_path"])
        batch_ids = entry["batch_ids"]

        statuses = [client.batches.retrieve(bid).status for bid in batch_ids]
        print(f"  {chunk_path.name} (retry): {statuses}")

        if all(s == "completed" for s in statuses):
            new_results: dict[str, str] = {}
            for bid in batch_ids:
                b = client.batches.retrieve(bid)
                if not b.output_file_id:
                    print(f"    WARNING: no output file for batch {bid}")
                    continue
                content = client.files.content(b.output_file_id).text
                for line in content.splitlines():
                    if not line.strip():
                        continue
                    try:
                        item = json.loads(line)
                        body = item["response"]["body"]
                        raw = json.loads(body["choices"][0]["message"]["content"])
                        for tid, cat in raw.get("labels", []):
                            if str(cat) in VALID_CATEGORIES:
                                new_results[str(tid)] = str(cat)
                    except Exception:
                        pass

            if chunk_path.exists():
                df = pd.read_parquet(chunk_path)
            else:
                src_path = entry.get("src_chunk_path", str(INCIV_DIR / chunk_path.name))
                df = pd.read_parquet(src_path)
                if "retweet_id" in df.columns:
                    df = df[df["retweet_id"].isna()].copy()
                df["topic_v2"] = None

            before_missing = df["topic_v2"].isna().sum()
            filled = df["id"].astype(str).map(new_results)
            df["topic_v2"] = df["topic_v2"].fillna(filled)
            after_missing = df["topic_v2"].isna().sum()
            newly_filled = before_missing - after_missing

            df.to_parquet(chunk_path, index=False, compression="snappy")

            print(f"    MERGED {chunk_path.name}: filled {newly_filled:,} tweets "
                  f"({after_missing:,} still missing)")
            state[name]["status"] = "completed"
            save_state(state)

        elif any(s in ("failed", "expired", "cancelled") for s in statuses):
            print(f"  PROBLEM: {name} → {statuses}")
            state[name]["status"] = "failed"
            save_state(state)

    done = sum(1 for v in state.values() if v["status"] == "completed")
    infly = sum(1 for v in state.values() if v["status"] == "submitted")
    total_missing = sum(
        pd.read_parquet(TOPICS_DIR / n, columns=["topic_v2"])["topic_v2"].isna().sum()
        for n in all_chunk_names
        if (TOPICS_DIR / n).exists()
    )
    print(f"\nRetry status: {done} done, {infly} in flight")
    print(f"Total unclassified tweets remaining across all chunks: {total_missing:,}")


if __name__ == "__main__":
    main()
