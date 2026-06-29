"""
Draws a stratified random sample of tweets for human validation of the
zero-shot topic classifier, at this stage still the original 12-category
scheme (later merged down to 9 -- see reclassify_topics_batch.py).

Stratified evenly across the 12 categories (Crime, Democracy, Economic,
Education, Environment, Identity, Migration, Nationalism, Other, Security,
Technology, Welfare), 50 tweets each, 600 total, with language family
(English / Germanic / Romance / Other) sampled proportionally to corpus
share within each topic so multilingual performance gets tested too.

Writes a master file with LLM topic + metadata, a coder-facing version
with the LLM topic hidden, and per-coder assignment files (200 tweets
shared for inter-coder reliability, 200 unique to each coder).

    python draw_topic_validation_sample.py [--seed 42]

Requires the CARDSPACE drive mounted and categorizations_of_posts.df.pkl
present at the path given in TOPICS_FILE below.
"""

from __future__ import annotations

import argparse
import math
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq

# Config -- edit these to point at your local copies of the raw data
CHUNKS_DIR  = Path("/Volumes/CARDSPACE/2026toxic/incivility_unique_run/incivility_final")
TOPICS_FILE = Path("/path/to/categorizations_of_posts.df.pkl")
OUTDIR      = Path("validation_sample")
OUTDIR.mkdir(exist_ok=True)

# Columns to load from tweet chunks
KEEP_COLS = [
    "id", "text", "lang", "country", "party", "from_user_realname",
    "created_at", "retweet_id", "party_mf_parfam", "partyfactsid",
    "incivility_llm",
]

# Topic category mapping (numeric code → label)
TOPIC_LABELS = {
    1:  "Economic",
    2:  "Welfare",
    3:  "Migration",
    4:  "Democracy",
    5:  "Security",
    6:  "Environment",
    7:  "Identity",
    8:  "Nationalism",
    9:  "Technology",
    10: "Crime",
    11: "Education",
    12: "Other",
    13: "Other",   # merged into Other
}
VALID_TOPICS = sorted(set(TOPIC_LABELS.values()))   # 12 unique labels

# Target sample per topic (× 12 = 600 total)
N_PER_TOPIC = 50

# Language family mapping
LANG_FAMILY = {
    "en": "English",
    "de": "Germanic", "nl": "Germanic", "sv": "Germanic",
    "da": "Germanic", "no": "Germanic", "nb": "Germanic",
    "nn": "Germanic", "is": "Germanic", "lb": "Germanic",
    "fr": "Romance",  "es": "Romance",  "it": "Romance",
    "pt": "Romance",  "ca": "Romance",  "gl": "Romance",
    "ro": "Romance",
}
LANG_FAMILIES = ["English", "Germanic", "Romance", "Other"]

# ParlGov family labels
FAM_LABELS = {
    10: "Green/Ecologist", 20: "Radical left",      30: "Social democrat",
    40: "Liberal",         50: "Christian democrat", 60: "Conservative",
    70: "Nationalist/Radical right", 80: "Agrarian",
    90: "Ethnic/Regional", 95: "Single issue",       98: "Other",
}

# Coder-facing columns
CODER_COLS = [
    "sample_id", "text", "date", "lang", "country",
    "party", "from_user_realname", "party_family",
    "coder_topic", "notes",
]


def assign_lang_family(lang_series: pd.Series) -> pd.Series:
    return lang_series.map(LANG_FAMILY).fillna("Other")


def load_chunk_with_topics(path: Path, topics_lookup: pd.Series) -> pd.DataFrame:
    """
    Load one chunk, attach LLM topic labels, drop retweets and missing text/topic.
    """
    available = pq.read_schema(path).names
    cols = [c for c in KEEP_COLS if c in available]
    df = pd.read_parquet(path, columns=cols)

    # Drop retweets
    if "retweet_id" in df.columns:
        df = df[df["retweet_id"].isna()].drop(columns=["retweet_id"])

    # Drop missing text or incivility score
    df = df.dropna(subset=["text"])

    # Attach topic labels
    df = df[df["id"].isin(topics_lookup.index)].copy()
    df["topic_code"] = topics_lookup.reindex(df["id"]).values
    df = df.dropna(subset=["topic_code"])
    df["topic_code"] = df["topic_code"].astype(int)
    df["topic"] = df["topic_code"].map(TOPIC_LABELS)
    df = df.dropna(subset=["topic"])

    # Language family
    if "lang" in df.columns:
        df["lang_family"] = assign_lang_family(df["lang"])
    else:
        df["lang_family"] = "Other"

    return df


def main(seed: int = 42):
    rng = np.random.default_rng(seed)

    # Load topic lookup
    print(f"Loading topic classifications from {TOPICS_FILE} …")
    topics = pd.read_pickle(TOPICS_FILE)
    # Keep only valid categories (1–13)
    topics = topics[topics["category"].isin(TOPIC_LABELS.keys())].copy()
    # IDs are stored as strings in the topics file but as int64 in parquet chunks
    topics["id_int"] = pd.to_numeric(topics["id"], errors="coerce")
    topics = topics.dropna(subset=["id_int"])
    topics["id_int"] = topics["id_int"].astype(np.int64)
    # Drop duplicates on the integer ID (covers both string dupes and conversion collisions)
    topics = topics.drop_duplicates(subset=["id_int"])
    topics = topics.set_index("id_int")["category"]
    print(f"  {len(topics):,} tweets with valid topic labels (unique int IDs)")

    # Load and filter chunks
    chunks = sorted(CHUNKS_DIR.glob("m_slim_*.parquet"))
    if not chunks:
        raise FileNotFoundError(f"No parquet chunks found in {CHUNKS_DIR}")
    print(f"Found {len(chunks)} chunks. Sampling tweets with topic labels …")

    # Pass 1: count per (topic, lang_family) across chunks
    print("\nPass 1: counting strata …")
    counts = {t: {f: 0 for f in LANG_FAMILIES} for t in VALID_TOPICS}
    for i, chunk in enumerate(chunks):
        try:
            df = load_chunk_with_topics(chunk, topics)
        except Exception as e:
            print(f"  Warning: chunk {chunk.name} failed ({e}); skipping")
            continue
        for topic in VALID_TOPICS:
            for fam in LANG_FAMILIES:
                counts[topic][fam] += ((df["topic"] == topic) &
                                        (df["lang_family"] == fam)).sum()
        if (i + 1) % 5 == 0 or (i + 1) == len(chunks):
            print(f"  … {i+1}/{len(chunks)} chunks counted")

    print("\nStrata counts:")
    ct_rows = []
    for topic in VALID_TOPICS:
        for fam in LANG_FAMILIES:
            ct_rows.append({"topic": topic, "lang_family": fam,
                             "n_available": counts[topic][fam]})
    ct = pd.DataFrame(ct_rows)
    print(ct.pivot(index="topic", columns="lang_family",
                   values="n_available").to_string())

    # Compute per-chunk targets (proportional + overshoot)
    overshoot = 1.5

    per_chunk_targets: dict[int, dict[str, dict[str, int]]] = {}
    for topic in VALID_TOPICS:
        n_topic_total = sum(counts[topic].values())
        for fam in LANG_FAMILIES:
            total_fam = counts[topic][fam]
            if total_fam == 0 or n_topic_total == 0:
                for i in range(len(chunks)):
                    per_chunk_targets.setdefault(i, {}) \
                        .setdefault(topic, {})[fam] = 0
                continue
            fam_share = total_fam / n_topic_total
            n_fam_target = max(1, round(N_PER_TOPIC * fam_share * overshoot))
            n_per_chunk = math.ceil(n_fam_target / len(chunks))
            for i in range(len(chunks)):
                per_chunk_targets.setdefault(i, {}) \
                    .setdefault(topic, {})[fam] = n_per_chunk

    # Pass 2: sample from each chunk
    print("\nPass 2: sampling …")
    collected = []
    for i, chunk in enumerate(chunks):
        try:
            df = load_chunk_with_topics(chunk, topics)
        except Exception as e:
            print(f"  Warning: chunk {chunk.name} failed; skipping")
            continue
        t_targets = per_chunk_targets.get(i, {})
        rows = []
        for topic in VALID_TOPICS:
            fam_targets = t_targets.get(topic, {})
            sub_t = df[df["topic"] == topic]
            for fam, n in fam_targets.items():
                if n == 0:
                    continue
                sub = sub_t[sub_t["lang_family"] == fam]
                if len(sub) == 0:
                    continue
                drawn = sub.sample(n=min(n, len(sub)), replace=False,
                                   random_state=int(rng.integers(1 << 31)))
                rows.append(drawn)
        if rows:
            collected.append(pd.concat(rows, ignore_index=True))
        if (i + 1) % 5 == 0 or (i + 1) == len(chunks):
            total_so_far = sum(len(c) for c in collected)
            print(f"  … {i+1}/{len(chunks)} chunks sampled ({total_so_far:,} rows)")

    pool = pd.concat(collected, ignore_index=True)
    pool = pool.drop_duplicates(subset=["id"])
    print(f"\nPool after dedup: {len(pool):,} rows")

    # Final stratified draw: exactly N_PER_TOPIC per topic
    print("Drawing final sample …")
    final_parts = []
    for topic in VALID_TOPICS:
        n_target = N_PER_TOPIC
        sub = pool[pool["topic"] == topic].copy()
        topic_total = sum(counts[topic].values())
        fam_parts = []
        assigned = 0
        for j, fam in enumerate(LANG_FAMILIES):
            if j == len(LANG_FAMILIES) - 1:
                n_draw = n_target - assigned
            else:
                fam_share = counts[topic][fam] / max(topic_total, 1)
                n_draw = round(n_target * fam_share)
            n_draw = max(0, n_draw)
            assigned += n_draw
            sub_fam = sub[sub["lang_family"] == fam]
            if len(sub_fam) >= n_draw:
                fam_parts.append(sub_fam.sample(
                    n=n_draw, replace=False,
                    random_state=int(rng.integers(1 << 31))))
            else:
                print(f"  WARNING: topic={topic}, lang={fam}: "
                      f"only {len(sub_fam)} available (target {n_draw})")
                fam_parts.append(sub_fam)
                already_used = pd.concat(fam_parts).index
                others = sub[~sub.index.isin(already_used)]
                n_backfill = min(n_draw - len(sub_fam), len(others))
                if n_backfill > 0:
                    fam_parts.append(others.sample(
                        n=n_backfill, replace=False,
                        random_state=int(rng.integers(1 << 31))))
        if fam_parts:
            final_parts.append(pd.concat(fam_parts, ignore_index=True))

    final = pd.concat(final_parts, ignore_index=True)
    before_dedup = len(final)
    final = final.drop_duplicates(subset=["id"])
    if len(final) < before_dedup:
        print(f"  Removed {before_dedup - len(final)} duplicate IDs")

    # Shuffle and annotate
    final = final.sample(frac=1, random_state=seed).reset_index(drop=True)
    final.index.name = "sample_id"
    final = final.reset_index()

    final["party_family"] = (pd.to_numeric(final["party_mf_parfam"], errors="coerce")
                              .map(FAM_LABELS).fillna("Unknown"))
    final["date"] = pd.to_datetime(final["created_at"]).dt.to_period("M").astype(str)

    # Summary
    print("\nFinal sample composition:")
    print(final.groupby(["topic", "lang_family"]).size().unstack(fill_value=0).to_string())
    print(f"\nTotal: {len(final):,} tweets, "
          f"{final['country'].nunique()} countries, "
          f"{final['lang'].nunique()} languages")

    # Master and coder files
    MASTER_COLS = [
        "sample_id", "id", "text", "date", "lang", "lang_family",
        "country", "party", "from_user_realname", "party_family",
        "topic",          # LLM-assigned topic — HIDE FROM CODERS
        "incivility_llm", # LLM incivility score — HIDE FROM CODERS
    ]
    master = final[[c for c in MASTER_COLS if c in final.columns]]

    coders_base_cols = [c for c in CODER_COLS[:-2] if c in final.columns]
    coders = final[coders_base_cols].copy()
    coders["coder_topic"] = ""
    coders["notes"] = ""

    today = date.today().isoformat()
    master_path = OUTDIR / f"topic_validation_full_{today}.csv"
    coder_path  = OUTDIR / f"topic_validation_coders_{today}.csv"

    master.to_csv(master_path, index=False)
    coders.to_csv(coder_path,  index=False)

    print(f"\n  saved → {master_path}  (master, includes LLM topic)")
    print(f"  saved → {coder_path}  (for coders, LLM topic removed)")

    # Split into coder assignments
    # 200 overlap (both coders) + 200 unique per coder = 400 per coder
    # Overlap: stratified proportionally by topic (~17/topic, use 16-17 per topic)
    OVERLAP_PER_TOPIC = max(1, round(200 / len(VALID_TOPICS)))  # ≈ 17

    overlap_idx = []
    for topic in VALID_TOPICS:
        pool_t = final[final["topic"] == topic].index.tolist()
        n = min(OVERLAP_PER_TOPIC, len(pool_t))
        chosen = rng.choice(pool_t, size=n, replace=False)
        overlap_idx.extend(chosen.tolist())

    overlap_mask = final.index.isin(overlap_idx)
    remainder    = final[~overlap_mask].copy()

    # Split remainder 50/50 stratified by topic
    c1_idx, c2_idx = [], []
    for topic in VALID_TOPICS:
        pool_t = remainder[remainder["topic"] == topic].index.tolist()
        rng.shuffle(pool_t)
        mid = len(pool_t) // 2
        c1_idx.extend(pool_t[:mid])
        c2_idx.extend(pool_t[mid:])

    final["assignment"] = "unassigned"
    final.loc[overlap_mask,               "assignment"] = "overlap"
    final.loc[final.index.isin(c1_idx),   "assignment"] = "coder1_unique"
    final.loc[final.index.isin(c2_idx),   "assignment"] = "coder2_unique"

    print(f"\nAssignment breakdown:")
    print(final["assignment"].value_counts().to_string())

    def make_coder_file(unique_idx: list) -> pd.DataFrame:
        rows = final[overlap_mask | final.index.isin(unique_idx)].copy()
        rows = rows.sample(frac=1, random_state=int(rng.integers(1 << 31)))
        rows = rows.reset_index(drop=True)
        rows.index.name = "row_id"
        rows = rows.reset_index()
        out = rows[[c for c in coders_base_cols]].copy()
        out["coder_topic"] = ""
        out["notes"]       = ""
        return out

    c1 = make_coder_file(c1_idx)
    c2 = make_coder_file(c2_idx)

    c1_path    = OUTDIR / "topic_coder1_assignments.csv"
    c2_path    = OUTDIR / "topic_coder2_assignments.csv"
    master_out = OUTDIR / "topic_assignment_master.csv"

    c1.to_csv(c1_path,    index=False)
    c2.to_csv(c2_path,    index=False)
    final.to_csv(master_out, index=False)

    print(f"\n  saved → {c1_path}  ({len(c1)} tweets for coder 1)")
    print(f"  saved → {c2_path}  ({len(c2)} tweets for coder 2)")
    print(f"  saved → {master_out}  (analyst master with LLM topic + assignment flags)")
    print("\nReminder: do NOT share topic_assignment_master.csv or "
          "topic_validation_full_*.csv with coders.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Draw stratified topic validation sample")
    parser.add_argument("--seed", type=int, default=42)
    args = parser.parse_args()
    main(seed=args.seed)
