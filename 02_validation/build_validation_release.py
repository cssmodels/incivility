"""
Builds the public-release version of the human-coded validation sample:
the same 1,200-tweet stratified sample and coder judgments behind the
paper's validation kappa, with the raw tweet text removed.

Tweet text is dropped for the same reason the full corpus is shared as
IDs rather than text: Twitter/X's terms restrict redistributing tweet
content in bulk. The tweet `id` column is kept, so the original text can
be re-hydrated by anyone who wants it, subject to those same terms.

Each coder's score is kept in its own column (coder1_score .. coder4_score)
rather than only publishing the deduplicated value actually used in the
paper, so the underlying judgments remain available for independent
reanalysis. human_score_final is the value the paper's kappa statistics
were computed on: Coder 1 and Coder 2 each cover a broad language pool;
the Turkish and Greek specialists (Coder 3, Coder 4) are used only for
the items in their respective native-language-only pool (lang_group ==
'c3_only' / 'c4_only') -- elsewhere their items overlap the Coder 1/2
pool and are not double-counted. Priority where more than one coder
scored the same tweet: Coder 1 > Coder 2 > Coder 3 (native pool only) >
Coder 4 (native pool only).

Usage
-----
    python build_validation_release.py

Output
------
    data_release/validation_sample_public.csv
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
PROJECT_ROOT = REPO_ROOT.parent
VAL_DIR = PROJECT_ROOT / "validation_sample"
CODING = VAL_DIR / "finished_coding"
OUT_DIR = PROJECT_ROOT / "data_release"
OUT_DIR.mkdir(exist_ok=True)

KEEP_MASTER_COLS = [
    "id", "date", "lang", "lang_family", "country", "party",
    "from_user_realname", "party_family", "in_government",
    "populism_score", "incivility_llm", "lang_group", "assignment_v2",
]

CODER_FILES = {
    "coder1_score": (CODING / "coder1_assignment_v2.csv", "coder_score", None),
    "coder2_score": (CODING / "coder2_assignments_v2.csv", "coder_score", None),
    "coder3_score": (CODING / "coder3_turkish_assignments_correct_new.csv", "human_score", "c3_only"),
    "coder4_score": (CODING / "coder4_greek_assignments_correct.csv", "coder_score", "c4_only"),
}


def main() -> None:
    master = pd.read_csv(VAL_DIR / "assignment_master_v2.csv").set_index("sample_id")
    out = master[KEEP_MASTER_COLS].copy()
    out = out.rename(columns={"incivility_llm": "incivility_llm_design"})

    llm = pd.read_csv("eval_results/scores_cb_gpt_4.1.csv", encoding="utf-8-sig")
    out["llm_score_gpt4.1"] = llm.set_index("sample_id")["llm_new"]

    coder_cols = {}
    for out_col, (path, score_col, restrict_lang_group) in CODER_FILES.items():
        coder = pd.read_csv(path, encoding="utf-8-sig").set_index("sample_id")[score_col]
        if restrict_lang_group is not None:
            in_pool = master["lang_group"] == restrict_lang_group
            coder = coder[coder.index.isin(master.index[in_pool])]
        out[out_col] = coder
        coder_cols[out_col] = coder

    # Deduplicated value actually used in the paper's kappa statistics:
    # priority coder1 > coder2 > coder3 (native pool) > coder4 (native pool).
    final = pd.Series(dtype=float)
    for col in ["coder4_score", "coder3_score", "coder2_score", "coder1_score"]:
        s = coder_cols[col]
        final = s.copy() if final.empty else final.combine_first(s)
    for col in ["coder4_score", "coder3_score", "coder2_score", "coder1_score"]:
        final.update(coder_cols[col])
    out["human_score_final"] = final

    out = out.reset_index().sort_values("sample_id")
    outfile = OUT_DIR / "validation_sample_public.csv"
    out.to_csv(outfile, index=False)

    n_scored = out["human_score_final"].notna().sum()
    n_llm = out["llm_score_gpt4.1"].notna().sum()
    n_both = (out["human_score_final"].notna() & out["llm_score_gpt4.1"].notna()).sum()
    print(f"Wrote {len(out)} rows -> {outfile}")
    print(f"  human_score_final populated: {n_scored}")
    print(f"  llm_score_gpt4.1 populated:   {n_llm}")
    print(f"  both populated (used for kappa): {n_both}")


if __name__ == "__main__":
    main()
