"""
Three descriptive figures built straight from the panel, no model fit
needed: fig_score_stacked.pdf (share of tweets in each incivility band by
month, civil / mildly uncivil / very-to-extremely uncivil, using y_any and
y_strong), fig_threshold_trends.pdf (the >=1 vs >=2 trend side by side,
showing that >=2 only captures the tail), and fig_score_stacked_byfamily.pdf
(the same stacked bands faceted by party family).

    python fig_score_distribution.py [--data PATH] [--outdir PATH]
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

import matplotlib as mpl
import matplotlib.dates as mdates
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
        "axes.axisbelow": True,
        "axes.grid": False,
        "xtick.direction": "out",
        "ytick.direction": "out",
    })


DOUBLE_W = 7.10   # ~180 mm
SINGLE_W = 3.35   # ~85 mm

# Colour palette: civil (light), mild (mid), severe (dark)
BAND_COLORS = ["#d4e6f1", "#85c1e9", "#1a5276"]
BAND_LABELS = ["Civil (score 0)", "Mildly uncivil (score 1)", "Very/extremely uncivil (score ≥2)"]

PARFAM_LABELS = {
    10: "Green/Ecologist",
    20: "Radical left",
    30: "Social democrat",
    40: "Liberal",
    50: "Christian democrat",
    60: "Conservative",
    70: "Nationalist/Radical right",
    80: "Agrarian",
    90: "Ethnic/Regional",
    95: "Single issue",
    98: "Other",
}
# String codes sometimes appear in panels built before numeric mapping
PARFAM_LABELS_STR = {
    "ECO": "Green/Ecologist",
    "LEF": "Radical left",
    "SOC": "Social democrat",
    "LIB": "Liberal",
    "CHR": "Christian democrat",
    "CON": "Conservative",
    "NAT": "Nationalist/Radical right",
    "AGR": "Agrarian",
    "ETH": "Ethnic/Regional",
    "SIP": "Single issue",
    "DIV": "Other",
}


def save_fig(fig: plt.Figure, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path)
    plt.close(fig)
    print(f"  saved → {path}")


def load_panel(path: Path) -> pd.DataFrame:
    if path.suffix == ".pkl":
        pt = pd.read_pickle(path)
    else:
        pt = pd.read_parquet(path)
    pt["month"] = pd.to_datetime(pt["month"])
    return pt


def compute_monthly_bands(pt: pd.DataFrame) -> pd.DataFrame:
    """
    Aggregate to monthly totals and derive three score bands.
    Returns a DataFrame indexed by month with columns:
        n_tweets, y_any, y_strong, y_mild, y_civil,
        frac_civil, frac_mild, frac_severe
    """
    m = (
        pt.groupby("month")
        .agg(n_tweets=("n_tweets", "sum"),
             y_any=("y_any", "sum"),
             y_strong=("y_strong", "sum"))
        .sort_index()
    )
    m["y_mild"]   = m["y_any"] - m["y_strong"]   # score exactly 1
    m["y_civil"]  = m["n_tweets"] - m["y_any"]   # score 0

    m["frac_civil"]  = m["y_civil"]  / m["n_tweets"]
    m["frac_mild"]   = m["y_mild"]   / m["n_tweets"]
    m["frac_severe"] = m["y_strong"] / m["n_tweets"]
    return m


# Figure 1: stacked area chart

def fig_score_stacked(pt: pd.DataFrame, outfile: Path) -> None:
    """
    Stacked area chart showing share at each incivility band over time.
    Y-axis shows % of all tweets. Bands from bottom: civil | mild | severe.
    """
    m = compute_monthly_bands(pt)

    fig, ax = plt.subplots(figsize=(DOUBLE_W, DOUBLE_W * 0.50))

    ax.stackplot(
        m.index,
        m["frac_civil"]  * 100,
        m["frac_mild"]   * 100,
        m["frac_severe"] * 100,
        labels=BAND_LABELS,
        colors=BAND_COLORS,
        alpha=0.90,
    )

    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.set_xlim(m.index[0], m.index[-1])
    ax.set_ylim(0, 100)
    ax.set_xlabel("Month")
    ax.set_ylabel("Share of all tweets (%)")
    ax.set_title("Distribution of incivility scores in parliamentary tweets (2017–2022)")

    # Legend — reverse order so severe is at the top entry, matching the top of the stack
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles[::-1], labels[::-1], loc="upper left", fontsize=8, frameon=False)

    # Annotate start and end values for each band
    for frac_col, color, va, offset in [
        ("frac_severe", BAND_COLORS[2], "bottom", 0.5),
        ("frac_mild",   BAND_COLORS[1], "center", 0),
        ("frac_civil",  BAND_COLORS[0], "top",   -0.5),
    ]:
        v_start = m[frac_col].iloc[:3].mean() * 100
        v_end   = m[frac_col].iloc[-3:].mean() * 100
        # Compute cumulative position for mid-annotation
        if frac_col == "frac_severe":
            y_mid_start = m["frac_mild"].iloc[:3].mean() * 100 + v_start / 2
            y_mid_end   = m["frac_mild"].iloc[-3:].mean() * 100 + v_end / 2
        elif frac_col == "frac_mild":
            y_mid_start = v_start / 2
            y_mid_end   = v_end / 2
        else:  # civil
            y_mid_start = (m["frac_mild"].iloc[:3].mean() + m["frac_severe"].iloc[:3].mean()) * 100 + v_start / 2
            y_mid_end   = (m["frac_mild"].iloc[-3:].mean() + m["frac_severe"].iloc[-3:].mean()) * 100 + v_end / 2

    fig.tight_layout()
    save_fig(fig, outfile)

    print("\nScore band summary:")
    for label, col in [("Civil (0)",        "frac_civil"),
                        ("Mild (1)",          "frac_mild"),
                        ("Severe (≥2)",       "frac_severe")]:
        start = m[col].iloc[:3].mean() * 100
        end   = m[col].iloc[-3:].mean() * 100
        print(f"  {label:20s}  start={start:.1f}%  end={end:.1f}%  Δ={end-start:+.2f}pp")


# Figure 2: side-by-side threshold comparison

def fig_threshold_trends(pt: pd.DataFrame, outfile: Path) -> None:
    """
    Two-panel figure: left = ≥1 ("any incivility") trend, right = ≥2 ("severe").
    Both panels have LOWESS smoothers and matching y-axis annotations.
    Directly supports the argument that ≥2 is the tail of a much larger phenomenon.
    """
    from statsmodels.nonparametric.smoothers_lowess import lowess

    m = compute_monthly_bands(pt)
    months_num = (m.index - m.index[0]).days.astype(float).values

    fig, axes = plt.subplots(1, 2, figsize=(DOUBLE_W, DOUBLE_W * 0.45), sharey=False)

    panels = [
        ("y_any",    "n_tweets", "Any incivility (score ≥1)",   "Share of tweets with\nany incivility (%)"),
        ("y_strong", "n_tweets", "Severe incivility (score ≥2)", "Share of tweets with\nsevere incivility (%)"),
    ]

    for ax, (y_col, n_col, title, ylabel) in zip(axes, panels):
        rate = m[y_col] / m[n_col]
        sm   = lowess(rate.values, months_num, frac=0.25, return_sorted=True)
        sm_dates = m.index[0] + pd.to_timedelta(sm[:, 0], unit="D")

        ax.scatter(m.index, rate * 100,
                   s=m[n_col] / m[n_col].max() * 25,
                   color="black", alpha=0.5, zorder=3)
        ax.plot(sm_dates, sm[:, 1] * 100, color="black", linewidth=1.5)

        # Annotate first and last smoothed value
        ax.annotate(f"{sm[0, 1]*100:.1f}%", xy=(sm_dates[0], sm[0, 1]*100),
                    xytext=(5, 4), textcoords="offset points", fontsize=7)
        ax.annotate(f"{sm[-1, 1]*100:.1f}%", xy=(sm_dates[-1], sm[-1, 1]*100),
                    xytext=(-35, 4), textcoords="offset points", fontsize=7)

        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.set_xlabel("Month")
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        ax.set_ylim(bottom=0)

    fig.suptitle("Incivility trends at two thresholds (2017–2022)", y=1.02, fontsize=10)
    fig.tight_layout()
    save_fig(fig, outfile)


# Figure 3: stacked bands by party family

def fig_score_stacked_byfamily(pt: pd.DataFrame, outfile: Path) -> None:
    """
    Faceted stacked area chart — one panel per party family.
    Shows whether the composition of incivility differs across political families.
    """
    pf_col = None
    for c in ["party_mf_parfam", "partyfamily", "family_name_short"]:
        if c in pt.columns:
            pf_col = c
            break
    if pf_col is None:
        print("  [skip] No party-family column found in panel.")
        return

    def get_label(code) -> str:
        if isinstance(code, (int, float)):
            return PARFAM_LABELS.get(int(code), f"Other ({code})")
        return PARFAM_LABELS_STR.get(str(code), str(code))

    # Only keep families with enough data
    pt2 = pt.copy()
    pt2["pf_label"] = pt2[pf_col].apply(get_label)

    # Filter to families with at least 50k total tweets
    family_totals = pt2.groupby("pf_label")["n_tweets"].sum()
    big_families  = family_totals[family_totals >= 50_000].index.tolist()
    pt2 = pt2[pt2["pf_label"].isin(big_families)]

    families = sorted(pt2["pf_label"].unique())
    ncols = 3
    nrows = math.ceil(len(families) / ncols)

    fig, axes = plt.subplots(nrows, ncols,
                              figsize=(DOUBLE_W * 1.2, nrows * 2.0),
                              sharex=True, sharey=False)
    axes_flat = axes.flatten()

    for ax, fam in zip(axes_flat, families):
        sub = pt2[pt2["pf_label"] == fam]
        m = (
            sub.groupby("month")
            .agg(n_tweets=("n_tweets", "sum"),
                 y_any=("y_any", "sum"),
                 y_strong=("y_strong", "sum"))
            .sort_index()
        )
        m["y_mild"]  = m["y_any"] - m["y_strong"]
        m["y_civil"] = m["n_tweets"] - m["y_any"]

        ax.stackplot(
            m.index,
            m["y_civil"]  / m["n_tweets"] * 100,
            m["y_mild"]   / m["n_tweets"] * 100,
            m["y_strong"] / m["n_tweets"] * 100,
            colors=BAND_COLORS,
            alpha=0.90,
        )
        ax.set_title(fam, fontsize=7, pad=2)
        ax.xaxis.set_major_formatter(mdates.DateFormatter("'%y"))
        ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.tick_params(labelsize=6)
        ax.set_ylim(0, 100)

        # Annotate final severe % in top right
        final_severe = m["y_strong"].iloc[-3:].sum() / m["n_tweets"].iloc[-3:].sum() * 100
        ax.text(0.97, 0.95, f"≥2: {final_severe:.1f}%",
                transform=ax.transAxes, fontsize=6, ha="right", va="top",
                color=BAND_COLORS[2])

    for ax in axes_flat[len(families):]:
        ax.set_visible(False)

    # Shared legend
    from matplotlib.patches import Patch
    legend_elements = [Patch(facecolor=c, label=l, alpha=0.9)
                        for c, l in zip(BAND_COLORS[::-1], BAND_LABELS[::-1])]
    fig.legend(handles=legend_elements, loc="lower center",
               ncol=3, fontsize=7, bbox_to_anchor=(0.5, -0.02), frameon=False)

    fig.supxlabel("Year", fontsize=8)
    fig.supylabel("Share of tweets (%)", fontsize=8)
    fig.suptitle("Incivility score distribution by party family (2017–2022)", fontsize=9)
    fig.tight_layout(rect=[0, 0.04, 1, 1])
    save_fig(fig, outfile)


# CLI

def main():
    p = argparse.ArgumentParser(description="Score distribution figures (no model needed)")
    p.add_argument("--data",   default="2026aggregated/party_country_month_panel_v4.df.pkl",
                   help="Path to party × country × month panel (.pkl or .parquet)")
    p.add_argument("--outdir", default="results_v3", help="Output directory")
    args = p.parse_args()

    set_pub_style()
    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    print(f"Loading {args.data} …")
    pt = load_panel(Path(args.data))
    print(f"  {len(pt):,} obs, {pt['country'].nunique()} countries, "
          f"{pt['n_tweets'].sum():,.0f} total tweets")

    # Overall rate summary
    total = pt["n_tweets"].sum()
    print(f"\n  Rate ≥1 (any):    {pt['y_any'].sum()/total:.3%}")
    print(f"  Rate ≥2 (severe): {pt['y_strong'].sum()/total:.3%}")
    print(f"  Rate  1 (mild):   {(pt['y_any'].sum()-pt['y_strong'].sum())/total:.3%}")
    print(f"  Rate  0 (civil):  {(total-pt['y_any'].sum())/total:.3%}")

    print("\nGenerating figures …")
    fig_score_stacked(pt,           outdir / "fig_score_stacked.pdf")
    fig_threshold_trends(pt,        outdir / "fig_threshold_trends.pdf")
    fig_score_stacked_byfamily(pt,  outdir / "fig_score_stacked_byfamily.pdf")

    print("\nDone.")


if __name__ == "__main__":
    main()
