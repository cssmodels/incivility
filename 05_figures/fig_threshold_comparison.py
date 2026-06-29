"""
Builds a side-by-side forest plot comparing model estimates at the two
incivility thresholds -- severe (>=2, results_v3/effects_summary.csv) and
any (>=1, results_v4_yany/effects_summary.csv). Run once both model fits
have finished.

    python fig_threshold_comparison.py \\
        --strong results_v3/effects_summary.csv \\
        --any    results_v4_yany/effects_summary.csv \\
        --outdir results_v3

Writes fig_threshold_comparison.pdf and a merged threshold_comparison.csv.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# Publication style
def set_pub_style(base_font: int = 9) -> None:
    mpl.rcParams.update({
        "font.size": base_font,
        "axes.titlesize": base_font + 1,
        "axes.labelsize": base_font,
        "xtick.labelsize": base_font - 1,
        "ytick.labelsize": base_font - 1,
        "legend.fontsize": base_font - 1,
        "font.family": "sans-serif",
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
        "figure.dpi": 120,
        "savefig.dpi": 300,
        "savefig.bbox": "tight",
        "savefig.pad_inches": 0.02,
        "axes.linewidth": 0.8,
        "axes.spines.top": False,
        "axes.spines.right": False,
    })


DOUBLE_W = 7.10

# Effects to include (in display order)
EFFECT_ORDER = [
    "Time slope (global, per SD)",
    "Cabinet party",
    "Election period",
    "COVID shock",
    "Post-COVID period",
    "Populism index (+1 SD)",
    "Econ left–right (+1 SD)",
    "Right-wing populism interaction (+1 SD)",
    "Liberal democracy (+1 SD)",
]

# Shorter labels for the plot
SHORT_LABELS = {
    "Time slope (global, per SD)":               "Time trend (per SD)",
    "Cabinet party":                              "Cabinet party",
    "Election period":                            "Election period",
    "COVID shock":                                "COVID shock",
    "Post-COVID period":                          "Post-COVID period",
    "Populism index (+1 SD)":                     "Populism (+1 SD)",
    "Econ left–right (+1 SD)":                    "Econ right (+1 SD)",
    "Right-wing populism interaction (+1 SD)":    "Right-wing populism (+1 SD)",
    "Liberal democracy (+1 SD)":                  "Liberal democracy (+1 SD)",
}


def load_effects(path: Path, label: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df = df[df["effect"].isin(EFFECT_ORDER)].copy()
    df["threshold"] = label
    return df


def fig_threshold_comparison(strong: pd.DataFrame, any_: pd.DataFrame,
                              outfile: Path) -> None:
    merged = pd.concat([strong, any_], ignore_index=True)

    # Sort by the ≥2 OR for consistent ordering
    order_map = {e: i for i, e in enumerate(EFFECT_ORDER)}
    effects_present = [e for e in EFFECT_ORDER if e in merged["effect"].values]

    n = len(effects_present)
    y_base = np.arange(n) * 2.2   # spacing between effect groups

    offset = 0.4   # vertical offset between the two threshold dots

    fig, ax = plt.subplots(figsize=(DOUBLE_W, max(4.0, n * 0.55)))

    colors  = {"≥2 (severe)": "#1a5276", "≥1 (any)": "#85c1e9"}
    markers = {"≥2 (severe)": "o",       "≥1 (any)": "s"}
    offsets = {"≥2 (severe)": +offset,   "≥1 (any)": -offset}

    for threshold, df_t in merged.groupby("threshold"):
        col    = colors[threshold]
        marker = markers[threshold]
        off    = offsets[threshold]
        for j, eff in enumerate(effects_present):
            row = df_t[df_t["effect"] == eff]
            if row.empty:
                continue
            y_pos = y_base[j] + off
            lo = float(np.log(row["or_low"].values[0]))
            mid = float(np.log(row["or_mid"].values[0]))
            hi = float(np.log(row["or_high"].values[0]))
            ax.hlines(y_pos, lo, hi, linewidth=1.1, color=col, alpha=0.85)
            ax.plot(mid, y_pos, marker=marker, color=col, markersize=5, zorder=5,
                    label=threshold if j == 0 else "_")

    ax.axvline(0.0, linewidth=0.8, linestyle="--", color="gray", zorder=1)

    # Y-axis labels
    ax.set_yticks(y_base)
    ax.set_yticklabels([SHORT_LABELS.get(e, e) for e in effects_present], fontsize=8)

    # Add OR = 1 annotation on x-axis
    ax.set_xlabel("Effect on log-odds (95% CI)\n← lower incivility   OR=1 (no effect)   higher incivility →")

    handles, labels_ = ax.get_legend_handles_labels()
    ax.legend(handles, labels_, loc="lower right", fontsize=8, frameon=False,
              title="Threshold", title_fontsize=8)

    ax.set_title("Main model effects: severe incivility (≥2) vs. any incivility (≥1)")
    fig.tight_layout()
    path = Path(outfile)
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path)
    plt.close(fig)
    print(f"  saved → {path}")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--strong",  default="results_v3/effects_summary.csv",
                   help="Effects CSV for y_strong (≥2) model")
    p.add_argument("--any",     default="results_v4_yany/effects_summary.csv",
                   help="Effects CSV for y_any (≥1) model")
    p.add_argument("--outdir",  default="results_v3",
                   help="Output directory")
    args = p.parse_args()

    set_pub_style()
    outdir = Path(args.outdir)

    strong_path = Path(args.strong)
    any_path    = Path(args.any)

    if not strong_path.exists():
        raise FileNotFoundError(f"Strong model results not found: {strong_path}")
    if not any_path.exists():
        raise FileNotFoundError(f"Any model results not found: {any_path}")

    strong = load_effects(strong_path, "≥2 (severe)")
    any_   = load_effects(any_path,    "≥1 (any)")

    # Save merged table
    merged = pd.concat([strong, any_], ignore_index=True)
    csv_out = outdir / "threshold_comparison.csv"
    merged.to_csv(csv_out, index=False)
    print(f"  saved → {csv_out}")

    print("\nThreshold comparison: OR [95% CI]")
    print(f"{'Effect':<45}  {'≥2 OR':>10}  {'≥1 OR':>10}")
    print("-" * 70)
    for eff in EFFECT_ORDER:
        r2 = strong[strong["effect"] == eff]
        r1 = any_[any_["effect"] == eff]
        if r2.empty and r1.empty:
            continue
        s2 = f"{r2['or_mid'].values[0]:.2f} [{r2['or_low'].values[0]:.2f}–{r2['or_high'].values[0]:.2f}]" \
            if not r2.empty else "—"
        s1 = f"{r1['or_mid'].values[0]:.2f} [{r1['or_low'].values[0]:.2f}–{r1['or_high'].values[0]:.2f}]" \
            if not r1.empty else "—"
        print(f"  {eff:<43}  {s2:>22}  {s1:>22}")

    fig_threshold_comparison(strong, any_, outdir / "fig_threshold_comparison.pdf")
    print("\nDone.")


if __name__ == "__main__":
    main()
