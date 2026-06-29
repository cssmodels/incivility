"""
Draws a stratified random sample of tweets for human validation of the
incivility classifier (0-3 ordinal scale).

Stratified primarily by incivility_llm score, oversampling the 1/2 boundary
since that's the threshold the main analysis's DV is built on (score >= 2),
and where classifier errors matter most: 150 tweets each at scores 0 and 3
(easy cases, sampled lightly to confirm), 450 each at scores 1 and 2 (the
boundary, oversampled for power), 1,200 total. Within each score level,
sampling is proportional by language family (English / Germanic / Romance
/ Other). Retweets and tweets with missing text are excluded.

Writes two files to validation_sample/: a full version with incivility_llm
included (keep this one away from coders) and a coder-facing version with
the LLM score stripped and blank coder_score/notes columns added.

    python draw_validation_sample.py [--seed 42]
"""

import argparse
import math
from pathlib import Path
import numpy as np
import pandas as pd
import pyarrow.parquet as pq

# Config
CHUNKS_DIR = Path("/Volumes/CARDSPACE/2026toxic/incivility_unique_run/incivility_final")
OUTDIR     = Path("validation_sample")
OUTDIR.mkdir(exist_ok=True)

# Columns to keep from chunks (text is large; load only what's needed)
KEEP_COLS = [
    "id", "text", "lang", "country", "party", "from_user_realname",
    "created_at", "retweet_id", "party_mf_parfam", "partyfactsid",
    "cabinet_cabinet_party", "party_vd_v2xpa_popul", "incivility_llm",
]

# Language family mapping (tweet-level lang code → family)
LANG_FAMILY = {
    # English
    "en": "English",
    # Germanic
    "de": "Germanic", "nl": "Germanic", "sv": "Germanic",
    "da": "Germanic", "no": "Germanic", "nb": "Germanic",
    "nn": "Germanic", "is": "Germanic", "lb": "Germanic",
    # Romance
    "fr": "Romance",  "es": "Romance",  "it": "Romance",
    "pt": "Romance",  "ca": "Romance",  "gl": "Romance",
    "ro": "Romance",
}
LANG_FAMILIES = ["English", "Germanic", "Romance", "Other"]

# ParlGov family labels
FAM_LABELS = {
    10: "Green/Ecologist", 20: "Radical left", 30: "Social democrat",
    40: "Liberal", 50: "Christian democrat", 60: "Conservative",
    70: "Nationalist/Radical right", 80: "Agrarian",
    90: "Ethnic/Regional", 95: "Single issue", 98: "Other",
}


def assign_lang_family(lang_series: pd.Series) -> pd.Series:
    return lang_series.map(LANG_FAMILY).fillna("Other")


def load_and_filter_chunk(path: Path) -> pd.DataFrame:
    """Load one chunk, drop retweets and missing text, keep needed cols."""
    available = pq.read_schema(path).names
    cols = [c for c in KEEP_COLS if c in available]
    df = pd.read_parquet(path, columns=cols)
    # Drop retweets
    if "retweet_id" in df.columns:
        df = df[df["retweet_id"].isna()].copy()
    # Drop missing text or LLM score
    df = df.dropna(subset=["text", "incivility_llm"])
    df["incivility_llm"] = df["incivility_llm"].astype(int)
    df = df[df["incivility_llm"].isin([0, 1, 2, 3])]
    # Assign language family
    if "lang" in df.columns:
        df["lang_family"] = assign_lang_family(df["lang"])
    else:
        df["lang_family"] = "Other"
    return df


def proportional_chunk_sample(df: pd.DataFrame,
                               targets: dict,
                               rng: np.random.Generator) -> pd.DataFrame:
    """
    From one chunk, sample proportionally toward per-(score, lang_family)
    targets.  targets[score][lang_family] = how many to draw from this chunk.
    Returns a (possibly smaller) sample; we overshoot and trim later.
    """
    rows = []
    for score, fam_targets in targets.items():
        sub_score = df[df["incivility_llm"] == score]
        for fam, n in fam_targets.items():
            if n == 0:
                continue
            sub = sub_score[sub_score["lang_family"] == fam]
            if len(sub) == 0:
                continue
            drawn = sub.sample(n=min(n, len(sub)),
                               replace=False,
                               random_state=int(rng.integers(1 << 31)))
            rows.append(drawn)
    return pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()


# Boundary-weighted targets: oversample the 1/2 threshold region
N_PER_SCORE = {0: 150, 1: 450, 2: 450, 3: 150}   # sums to 1,200


def main(seed: int = 42):
    rng = np.random.default_rng(seed)

    chunks = sorted(CHUNKS_DIR.glob("m_slim_*.parquet"))
    if not chunks:
        raise FileNotFoundError(f"No parquet chunks found in {CHUNKS_DIR}")
    print(f"Found {len(chunks)} chunks")
    print(f"Target per score level: { {s: N_PER_SCORE[s] for s in range(4)} }")
    print(f"Total target: {sum(N_PER_SCORE.values())} tweets\n")

    # Pass 1: count per (score, lang_family) across all chunks
    print("Pass 1: counting strata across all chunks …")
    counts = {s: {f: 0 for f in LANG_FAMILIES} for s in range(4)}
    for i, chunk in enumerate(chunks):
        df = load_and_filter_chunk(chunk)
        for score in range(4):
            for fam in LANG_FAMILIES:
                counts[score][fam] += ((df["incivility_llm"] == score) &
                                       (df["lang_family"] == fam)).sum()
        if (i + 1) % 5 == 0 or (i + 1) == len(chunks):
            print(f"  … {i+1}/{len(chunks)} chunks counted")

    print("\nStrata counts (original tweets, excl. retweets):")
    rows_ct = []
    for score in range(4):
        for fam in LANG_FAMILIES:
            rows_ct.append({"score": score, "lang_family": fam,
                            "n_available": counts[score][fam]})
    ct = pd.DataFrame(rows_ct)
    print(ct.pivot(index="score", columns="lang_family",
                   values="n_available").to_string())

    # Compute per-chunk targets
    # For each (score, lang_family) cell, draw proportionally across chunks.
    # Overshoot by 30% and trim to exact target in the final draw.
    overshoot = 1.30

    per_chunk_targets = {}  # chunk_idx → {score: {fam: n}}
    for score in range(4):
        n_score = N_PER_SCORE[score]
        total_score = sum(counts[score].values())
        for fam in LANG_FAMILIES:
            total_fam = counts[score][fam]
            if total_fam == 0 or total_score == 0:
                for i in range(len(chunks)):
                    per_chunk_targets.setdefault(i, {}) \
                        .setdefault(score, {})[fam] = 0
                continue
            # Target for this (score, fam) cell, proportional to corpus share
            fam_share = total_fam / total_score
            n_fam_target = max(1, round(n_score * fam_share * overshoot))
            # Spread evenly across chunks
            n_per_chunk = math.ceil(n_fam_target / len(chunks))
            for i in range(len(chunks)):
                per_chunk_targets.setdefault(i, {}) \
                    .setdefault(score, {})[fam] = n_per_chunk

    # Pass 2: sample from each chunk
    print("\nPass 2: sampling from each chunk …")
    collected = []
    for i, chunk in enumerate(chunks):
        df = load_and_filter_chunk(chunk)
        sample = proportional_chunk_sample(df, per_chunk_targets[i], rng)
        collected.append(sample)
        if (i + 1) % 5 == 0 or (i + 1) == len(chunks):
            print(f"  … {i+1}/{len(chunks)} chunks sampled "
                  f"(running total: "
                  f"{sum(len(c) for c in collected):,} rows)")

    pool = pd.concat(collected, ignore_index=True)
    print(f"\nPool before final draw: {len(pool):,} rows")

    # Final stratified draw: exactly N_PER_SCORE[score] per level
    print("Drawing final sample …")
    final_parts = []
    for score in range(4):
        n_target = N_PER_SCORE[score]
        sub = pool[pool["incivility_llm"] == score].copy()
        # Within score level, sample proportional to corpus lang_family share
        score_total = sum(counts[score].values())
        fam_parts = []
        assigned = 0
        fams = LANG_FAMILIES[:]
        for j, fam in enumerate(fams):
            # Last family gets the remainder to hit exact total
            if j == len(fams) - 1:
                n_draw = n_target - assigned
            else:
                fam_share = counts[score][fam] / max(score_total, 1)
                n_draw = round(n_target * fam_share)
            n_draw = max(0, n_draw)
            assigned += n_draw
            sub_fam = sub[sub["lang_family"] == fam]
            if len(sub_fam) >= n_draw:
                fam_parts.append(sub_fam.sample(
                    n=n_draw, replace=False,
                    random_state=int(rng.integers(1 << 31))))
            else:
                print(f"  WARNING: score={score}, lang={fam}: "
                      f"only {len(sub_fam)} available (target {n_draw})")
                fam_parts.append(sub_fam)
                # Back-fill from same score, any family
                already_used = pd.concat(fam_parts).index
                others = sub[~sub.index.isin(already_used)]
                n_backfill = min(n_draw - len(sub_fam), len(others))
                if n_backfill > 0:
                    fam_parts.append(others.sample(
                        n=n_backfill, replace=False,
                        random_state=int(rng.integers(1 << 31))))
        final_parts.append(pd.concat(fam_parts, ignore_index=True))

    final = pd.concat(final_parts, ignore_index=True)

    # Drop any accidental duplicates (same tweet sampled from two chunks)
    before_dedup = len(final)
    final = final.drop_duplicates(subset=["id"])
    if len(final) < before_dedup:
        print(f"  Removed {before_dedup - len(final)} duplicate tweet IDs")

    # Shuffle rows (so coders don't see all score=0 first)
    final = final.sample(frac=1, random_state=seed).reset_index(drop=True)
    final.index.name = "sample_id"
    final = final.reset_index()

    # Clean up output columns
    final["party_family"] = (pd.to_numeric(final["party_mf_parfam"],
                                            errors="coerce")
                               .map(FAM_LABELS)
                               .fillna("Unknown"))
    final["in_government"] = final["cabinet_cabinet_party"].fillna(0).astype(int)
    final["populism_score"] = pd.to_numeric(final["party_vd_v2xpa_popul"],
                                             errors="coerce").round(3)
    final["date"] = pd.to_datetime(final["created_at"]).dt.to_period("M").astype(str)

    # Summary
    print("\nFinal sample composition:")
    print(final.groupby(["incivility_llm", "lang_family"])
               .size().unstack(fill_value=0).to_string())
    print(f"\nCountries: {final['country'].nunique()} "
          f"({', '.join(sorted(final['country'].unique()))})")
    print(f"Total tweets: {len(final):,}")

    # Output columns
    MASTER_COLS = [
        "sample_id",
        "id",               # tweet ID
        "text",             # tweet text
        "date",             # YYYY-MM (approx, not exact date)
        "lang",             # tweet language code
        "lang_family",      # language family
        "country",          # country
        "party",            # party name
        "from_user_realname",  # politician name
        "party_family",     # ParlGov family label
        "in_government",    # 1 = governing party
        "populism_score",   # V-Party populism index
        "incivility_llm",   # GPT-4o-mini score — HIDE FROM CODERS
    ]

    CODER_COLS = [
        "sample_id",
        "text",
        "date",
        "lang",
        "country",
        "party",
        "from_user_realname",
        "party_family",
        "coder_score",      # blank — for human annotation
        "notes",            # blank — for comments
    ]

    master = final[[c for c in MASTER_COLS if c in final.columns]]
    coders = final[[c for c in CODER_COLS[:-2] if c in final.columns]].copy()
    coders["coder_score"] = ""
    coders["notes"] = ""

    from datetime import date
    today = date.today().isoformat()
    master_path = OUTDIR / f"validation_sample_full_{today}.csv"
    coder_path  = OUTDIR / f"validation_sample_coders_{today}.csv"

    master.to_csv(master_path, index=False)
    coders.to_csv(coder_path, index=False)

    print(f"\n  saved → {master_path}  (master, includes LLM score)")
    print(f"  saved → {coder_path}  (for coders, LLM score removed)")

    # Quick precision/recall preview against equal-threshold
    print("\nLLM score distribution in final sample:")
    print(final["incivility_llm"].value_counts().sort_index())
    print("\nFor reference — binary thresholds:")
    print(f"  ≥1 (any uncivil):    {(final['incivility_llm'] >= 1).mean():.1%}")
    print(f"  ≥2 (very uncivil):   {(final['incivility_llm'] >= 2).mean():.1%}")
    print(f"  =3 (extremely):      {(final['incivility_llm'] == 3).mean():.1%}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Draw stratified validation sample")
    parser.add_argument("--seed", type=int, default=42,
                        help="Random seed (default: 42)")
    args = parser.parse_args()
    main(seed=args.seed)
