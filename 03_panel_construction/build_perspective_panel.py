"""
Aggregates per-tweet Perspective API toxicity scores to party x country x
month cells and merges onto the main panel, giving the dependent variable
for the SI Section 2 cross-instrument robustness check (beta-regression on
mean Perspective toxicity, independent of the GPT incivility classifier).

Output: 2026aggregated/perspective_panel_v1.parquet
"""

from pathlib import Path
import numpy as np
import pandas as pd
import pyarrow.feather as feather

# Paths
FEATHER = Path("/Volumes/CARDSPACE/DATA/DATA/TOXICITYDATA_mergedwithsources_2025.feather")
MAIN_PANEL = Path("2026aggregated/party_country_month_panel_v4.df.pkl")
OUTFILE = Path("2026aggregated/perspective_panel_v1.parquet")

print("Loading Perspective feather (selected columns) …")
cols = ["id", "toxicity", "country", "partyfactsid", "created_at"]
tbl  = feather.read_table(str(FEATHER), columns=cols, memory_map=True)
df   = tbl.to_pandas()
print(f"  Rows loaded: {len(df):,}")

df = df[df["country"] != "European Parliament"].copy()
df = df.dropna(subset=["toxicity", "partyfactsid", "created_at"])
df["toxicity"]      = pd.to_numeric(df["toxicity"], errors="coerce").clip(0.0, 1.0)
df["partyfactsid"]  = df["partyfactsid"].astype("int64")
df["month"]         = pd.to_datetime(df["created_at"]).dt.to_period("M").dt.to_timestamp()
df = df.dropna(subset=["toxicity", "month"])
print(f"  After cleaning: {len(df):,} tweets, {df['country'].nunique()} countries")

print("Aggregating to party × country × month …")
cell = (
    df.groupby(["partyfactsid", "country", "month"])
    .agg(
        n_tweets_persp  = ("toxicity", "count"),
        mean_toxicity   = ("toxicity", "mean"),
        share_tox_50    = ("toxicity", lambda x: (x > 0.50).mean()),
        share_tox_30    = ("toxicity", lambda x: (x > 0.30).mean()),
    )
    .reset_index()
)
print(f"  Cells: {len(cell):,}")
print(f"  mean_toxicity: median={cell['mean_toxicity'].median():.4f}, "
      f"p90={cell['mean_toxicity'].quantile(0.90):.4f}")

print("Merging onto main panel …")
pt = pd.read_pickle(str(MAIN_PANEL))
pt["month"] = pd.to_datetime(pt["month"])
pt["partyfactsid"] = pd.to_numeric(pt["partyfactsid"], errors="coerce").astype("Int64")
cell["partyfactsid"] = cell["partyfactsid"].astype("Int64")

merged = pt.merge(cell, on=["partyfactsid", "country", "month"], how="inner")
print(f"  Main panel rows:   {len(pt):,}")
print(f"  After inner merge: {len(merged):,}  "
      f"({len(merged)/len(pt):.1%} of main panel covered)")
print(f"  Countries: {merged['country'].nunique()}")
print(f"  Parties:   {merged['partyfactsid'].nunique()}")

# Keep mean_toxicity away from exact 0/1 for the beta-regression likelihood
eps = 1e-4
merged["mean_toxicity_clipped"] = merged["mean_toxicity"].clip(eps, 1.0 - eps)

OUTFILE.parent.mkdir(parents=True, exist_ok=True)
merged.to_parquet(str(OUTFILE), index=False)
print(f"\nSaved → {OUTFILE}  ({len(merged):,} rows × {merged.shape[1]} cols)")

print("\nCoverage by country:")
cov = merged.groupby("country").agg(
    cells=("month", "count"),
    mean_tox=("mean_toxicity", "mean"),
).sort_values("mean_tox", ascending=False)
print(cov.round(4).to_string())
