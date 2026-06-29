"""
Routes the incivility validation sample to coders by native language.

Coder 1 covers en/de/sv/da/no/nl/es/pt/it, Coder 2 covers en/es/pt/fr/it,
Coder 3 is the Turkish specialist (tr), Coder 4 the Greek specialist (el);
DeepL fills in for everything else. The shared en/es/pt/it pool between
Coders 1 and 2 supplies a 200-tweet overlap set (stratified by score) for
inter-coder kappa, split ~50/50 the rest of the way. German/Dutch/Nordic
goes entirely to Coder 1, French entirely to Coder 2. Coders 3 and 4 also
each get 40 English tweets drawn from that same C1/C2 overlap set, so
kappa across all four coders can be computed on those items. Everything
not covered by a native speaker (Polish, Finnish, Romanian, etc.) is split
between Coders 1 and 2 with instructions to use DeepL, noting the source
language in the 'notes' column.

Writes coder1_assignments_v2.csv, coder2_assignments_v2.csv,
coder3_turkish_assignments.csv, coder4_greek_assignments.csv, and
assignment_master_v2.csv (all 1,200 rows with assignment flags).
"""

from pathlib import Path
import pandas as pd
import numpy as np

VALDIR = Path("validation_sample")
SEED   = 42

# Language group definitions
LANGS_C1       = {"en", "de", "sv", "da", "no", "nb", "nn", "is", "nl", "es", "pt", "it"}
LANGS_C2       = {"en", "es", "pt", "fr", "it"}
LANGS_SHARED   = LANGS_C1 & LANGS_C2          # en, es, pt, it
LANGS_C1_ONLY  = LANGS_C1 - LANGS_SHARED      # de, nl, sv, da, no, nb, nn, is
LANGS_C2_ONLY  = LANGS_C2 - LANGS_SHARED      # fr
LANGS_C3       = {"tr"}
LANGS_C4       = {"el"}

# English tweets to include in C3 and C4 files (for cross-coder reliability)
N_EN_FOR_C3C4  = 40

# Overlap between C1 and C2 — drawn from shared languages, stratified by score
OVERLAP_PER_SCORE = {0: 25, 1: 75, 2: 75, 3: 25}   # sums to 200

# Coder-facing columns (no LLM score, no assignment)
CODER_COLS = [
    "sample_id", "text", "date", "lang", "country",
    "party", "from_user_realname", "party_family",
    "coder_score", "notes",
]


def make_coder_file(df_subset: pd.DataFrame, rng: np.random.Generator,
                    note_deepl_langs: set | None = None) -> pd.DataFrame:
    """Shuffle rows, add blank annotation columns, return coder-facing DataFrame."""
    rows = df_subset.copy()
    rows = rows.sample(frac=1, random_state=int(rng.integers(1 << 31)))
    rows = rows.reset_index(drop=True)
    rows.index.name = "row_id"
    rows = rows.reset_index()
    out_cols = [c for c in CODER_COLS[:-2] if c in rows.columns]
    out = rows[out_cols].copy()
    # Pre-fill notes for DeepL tweets so coders know to translate
    if note_deepl_langs:
        mask = rows["lang"].isin(note_deepl_langs) | (rows["lang"] == "und")
        out["coder_score"] = ""
        out["notes"] = ""
        out.loc[mask, "notes"] = "[DeepL: please translate before coding]"
    else:
        out["coder_score"] = ""
        out["notes"] = ""
    return out


def main():
    rng = np.random.default_rng(SEED)

    # Load master
    master_files = sorted(VALDIR.glob("validation_sample_full_*.csv"))
    if not master_files:
        raise FileNotFoundError("No master file found. Run draw_validation_sample.py first.")
    df = pd.read_csv(master_files[-1])
    print(f"Loaded {master_files[-1].name}: {len(df):,} tweets")

    # Classify each tweet by language group
    def lang_group(lang):
        if lang in LANGS_SHARED:   return "shared"
        if lang in LANGS_C1_ONLY:  return "c1_only"
        if lang in LANGS_C2_ONLY:  return "c2_only"
        if lang in LANGS_C3:       return "c3_only"
        if lang in LANGS_C4:       return "c4_only"
        return "deepl"

    df["lang_group"] = df["lang"].apply(lang_group)

    shared  = df[df["lang_group"] == "shared"].copy()
    c1_only = df[df["lang_group"] == "c1_only"].copy()
    c2_only = df[df["lang_group"] == "c2_only"].copy()
    c3_only = df[df["lang_group"] == "c3_only"].copy()
    c4_only = df[df["lang_group"] == "c4_only"].copy()
    deepl   = df[df["lang_group"] == "deepl"].copy()

    print(f"\nLanguage group sizes:")
    print(f"  Shared (C1+C2):  {len(shared):>4}   {df['lang'][df['lang_group']=='shared'].value_counts().to_dict()}")
    print(f"  C1-only:         {len(c1_only):>4}   {df['lang'][df['lang_group']=='c1_only'].value_counts().to_dict()}")
    print(f"  C2-only:         {len(c2_only):>4}   {df['lang'][df['lang_group']=='c2_only'].value_counts().to_dict()}")
    print(f"  C3 (Turkish):    {len(c3_only):>4}")
    print(f"  C4 (Greek):      {len(c4_only):>4}")
    print(f"  DeepL pool:      {len(deepl):>4}   (pl, fi, ro, lv, cy, sl, und, ...)")

    # Draw C1/C2 overlap from shared languages (stratified by score)
    overlap_idx = []
    for score, n in OVERLAP_PER_SCORE.items():
        pool = shared[shared["incivility_llm"] == score].index.tolist()
        if len(pool) < n:
            print(f"  WARNING: score={score} in shared langs: only {len(pool)} available "
                  f"(target {n}); taking all")
            n = len(pool)
        chosen = rng.choice(pool, size=n, replace=False)
        overlap_idx.extend(chosen.tolist())

    overlap_mask_shared = shared.index.isin(overlap_idx)
    shared_non_overlap  = shared[~overlap_mask_shared].copy()
    overlap_set         = shared[overlap_mask_shared].copy()

    print(f"\nC1/C2 overlap set: {len(overlap_set)} tweets")
    print(f"  Score distribution: {overlap_set['incivility_llm'].value_counts().sort_index().to_dict()}")
    print(f"  Language distribution: {overlap_set['lang'].value_counts().to_dict()}")

    # English subset for C3 and C4 (from overlap so 4-way κ is possible)
    en_overlap = overlap_set[overlap_set["lang"] == "en"]
    if len(en_overlap) < N_EN_FOR_C3C4:
        print(f"  WARNING: only {len(en_overlap)} English tweets in overlap "
              f"(target {N_EN_FOR_C3C4})")
        n_en = len(en_overlap)
    else:
        n_en = N_EN_FOR_C3C4

    # Stratify the English subset by score for C3/C4
    en_for_c3c4_idx = []
    for score in range(4):
        pool = en_overlap[en_overlap["incivility_llm"] == score].index.tolist()
        n_draw = max(1, round(n_en * (OVERLAP_PER_SCORE[score] / 200)))
        n_draw = min(n_draw, len(pool))
        if n_draw > 0:
            en_for_c3c4_idx.extend(
                rng.choice(pool, size=n_draw, replace=False).tolist())

    en_for_c3c4 = overlap_set.loc[en_for_c3c4_idx]
    print(f"\nEnglish overlap subset for C3/C4: {len(en_for_c3c4)} tweets")
    print(f"  Score distribution: {en_for_c3c4['incivility_llm'].value_counts().sort_index().to_dict()}")

    # Split shared non-overlap between C1 and C2 (stratified by score)
    c1_shared_idx, c2_shared_idx = [], []
    for score in range(4):
        pool = shared_non_overlap[
            shared_non_overlap["incivility_llm"] == score].index.tolist()
        rng.shuffle(pool)
        mid = len(pool) // 2
        c1_shared_idx.extend(pool[:mid])
        c2_shared_idx.extend(pool[mid:])

    c1_shared = shared_non_overlap.loc[c1_shared_idx]
    c2_shared = shared_non_overlap.loc[c2_shared_idx]

    # Split DeepL pool between C1 and C2 to balance workloads
    # Workload without DeepL:
    #   C1: overlap + c1_shared + c1_only = 200 + len(c1_shared) + len(c1_only)
    #   C2: overlap + c2_shared + c2_only = 200 + len(c2_shared) + len(c2_only)
    c1_base = len(overlap_set) + len(c1_shared) + len(c1_only)
    c2_base = len(overlap_set) + len(c2_shared) + len(c2_only)
    total_deepl = len(deepl)
    # Give more DeepL to the coder with lighter base workload
    c2_gets = round(total_deepl * c1_base / (c1_base + c2_base))
    c1_gets = total_deepl - c2_gets

    deepl_shuffled = deepl.sample(frac=1, random_state=int(rng.integers(1 << 31)))
    c1_deepl = deepl_shuffled.iloc[:c1_gets]
    c2_deepl = deepl_shuffled.iloc[c1_gets:]

    # Assemble final datasets
    # Identify all DeepL languages for pre-filling notes
    deepl_langs = set(deepl["lang"].unique())

    c1_all = pd.concat([overlap_set, c1_shared, c1_only, c1_deepl], ignore_index=True)
    c2_all = pd.concat([overlap_set, c2_shared, c2_only, c2_deepl], ignore_index=True)
    c3_all = pd.concat([c3_only, en_for_c3c4], ignore_index=True)
    c4_all = pd.concat([c4_only, en_for_c3c4], ignore_index=True)

    # Tag master
    df["assignment_v2"] = "unassigned"
    df.loc[df.index.isin(overlap_set.index),  "assignment_v2"] = "overlap_c1c2"
    df.loc[df.index.isin(c1_shared.index),    "assignment_v2"] = "c1_unique_shared"
    df.loc[df.index.isin(c2_shared.index),    "assignment_v2"] = "c2_unique_shared"
    df.loc[df.index.isin(c1_only.index),      "assignment_v2"] = "c1_unique_native"
    df.loc[df.index.isin(c2_only.index),      "assignment_v2"] = "c2_unique_native"
    df.loc[df.index.isin(c1_deepl.index),     "assignment_v2"] = "c1_deepl"
    df.loc[df.index.isin(c2_deepl.index),     "assignment_v2"] = "c2_deepl"
    df.loc[df.index.isin(c3_only.index),      "assignment_v2"] = "c3_native"
    df.loc[df.index.isin(c4_only.index),      "assignment_v2"] = "c4_native"
    # en_for_c3c4 rows are already tagged as overlap_c1c2 (they ARE in overlap_set)
    df["in_c3_file"] = df.index.isin(en_for_c3c4.index) | df.index.isin(c3_only.index)
    df["in_c4_file"] = df.index.isin(en_for_c3c4.index) | df.index.isin(c4_only.index)

    # Build and save coder files
    c1_file = make_coder_file(c1_all, rng, note_deepl_langs=deepl_langs)
    c2_file = make_coder_file(c2_all, rng, note_deepl_langs=deepl_langs)
    c3_file = make_coder_file(c3_all, rng)
    c4_file = make_coder_file(c4_all, rng)

    c1_path = VALDIR / "coder1_assignments_v2.csv"
    c2_path = VALDIR / "coder2_assignments_v2.csv"
    c3_path = VALDIR / "coder3_turkish_assignments.csv"
    c4_path = VALDIR / "coder4_greek_assignments.csv"
    master_path = VALDIR / "assignment_master_v2.csv"

    c1_file.to_csv(c1_path, index=False)
    c2_file.to_csv(c2_path, index=False)
    c3_file.to_csv(c3_path, index=False)
    c4_file.to_csv(c4_path, index=False)
    df.to_csv(master_path, index=False)

    # Summary
    print(f"\n{'─'*60}")
    print(f"  Coder 1  →  {len(c1_file):>4} tweets  ({c1_path.name})")
    print(f"    native langs : {sorted(LANGS_C1)}")
    print(f"    overlap      : {len(overlap_set)} (with C2)")
    print(f"    DeepL tweets : {len(c1_deepl)} (pre-flagged in notes)")

    print(f"\n  Coder 2  →  {len(c2_file):>4} tweets  ({c2_path.name})")
    print(f"    native langs : {sorted(LANGS_C2)}")
    print(f"    overlap      : {len(overlap_set)} (with C1)")
    print(f"    DeepL tweets : {len(c2_deepl)} (pre-flagged in notes)")

    print(f"\n  Coder 3 (Turkish)  →  {len(c3_file):>4} tweets  ({c3_path.name})")
    print(f"    Turkish tweets : {len(c3_only)}")
    print(f"    English tweets : {len(en_for_c3c4)} (subset of C1/C2 overlap for cross-coder κ)")

    print(f"\n  Coder 4 (Greek)  →  {len(c4_file):>4} tweets  ({c4_path.name})")
    print(f"    Greek tweets   : {len(c4_only)}")
    print(f"    English tweets : {len(en_for_c3c4)} (same subset as C3 — 4-way κ on English)")

    print(f"\n  Master   →  {master_path.name}  ({len(df)} rows, full metadata + assignments)")

    print(f"\n{'─'*60}")
    print(f"Cross-coder reliability design:")
    print(f"  C1 ↔ C2 : {len(overlap_set)} shared-lang tweets  (en/es/pt/it)")
    print(f"  C1 ↔ C3 : {len(en_for_c3c4)} English tweets")
    print(f"  C1 ↔ C4 : {len(en_for_c3c4)} English tweets  (same {len(en_for_c3c4)} as above)")
    print(f"  C2 ↔ C3 : {len(en_for_c3c4)} English tweets")
    print(f"  C2 ↔ C4 : {len(en_for_c3c4)} English tweets")
    print(f"  C3 ↔ C4 : {len(en_for_c3c4)} English tweets  (4-way overlap)")

    print(f"\nWorkload check:")
    print(f"  C1: {len(overlap_set)} overlap + {len(c1_shared)} shared-unique + "
          f"{len(c1_only)} native-unique + {len(c1_deepl)} DeepL = {len(c1_file)}")
    print(f"  C2: {len(overlap_set)} overlap + {len(c2_shared)} shared-unique + "
          f"{len(c2_only)} native-unique + {len(c2_deepl)} DeepL = {len(c2_file)}")

    print(f"\nDeepL pool languages (flagged in notes):")
    print(f"  {deepl['lang'].value_counts().to_dict()}")
    print("\nReminder: do NOT share assignment_master_v2.csv with coders.")


if __name__ == "__main__":
    main()
