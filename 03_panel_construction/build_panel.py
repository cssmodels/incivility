"""
Builds the aggregated party x country x month panels from the 27
tweet-level parquet chunks (on the external CARDSPACE drive), merging in
topic classifications and party/country covariates (V-Dem, ParlGov,
PartyFacts). Writes to 2026aggregated/:

    party_country_month_panel_v4.df.pkl / .parquet   main panel
    country_month_panel_v4.csv                        country x month panel
    party_country_month_topic_panel_v4.pkl             adds y_any/y_strong per topic

Run locally with CARDSPACE mounted: python build_panel.py
Paths are set in the block below.
"""

from __future__ import annotations

import os
import warnings
from pathlib import Path

import sys
import numpy as np
import numpy.core
import numpy.core.numeric
import numpy.core.multiarray
# The chunk parquets were pickled under numpy 2.x (numpy._core.*); this
# environment has numpy 1.26 (numpy.core.*). Alias the new module paths to
# the old ones so unpickling doesn't fail on the name change.
for _attr in ["numeric", "multiarray", "fromnumeric", "umath",
              "_multiarray_umath", "numerictypes", "arrayprint"]:
    _key = f"numpy.core.{_attr}"
    if _key in sys.modules:
        sys.modules.setdefault(f"numpy._core.{_attr}", sys.modules[_key])
import pandas as pd
from tqdm.auto import tqdm

warnings.filterwarnings("ignore")

# Paths -- edit CHUNKS_DIR/DATASETS_DIR if drives are mounted elsewhere
REPO_ROOT    = Path(__file__).resolve().parents[1]
CHUNKS_DIR   = Path("/Volumes/CARDSPACE/2026toxic/topics_v2")  # has topic_v2 + incivility_llm_v2
DATASETS_DIR = Path("/path/to/datasets")  # V-Party / ParlGov raw CSVs, see README
OUT_DIR      = REPO_ROOT / "2026aggregated"

# V-Dem and ParlGov
VPARTY_CSV   = DATASETS_DIR / "CPD_V-Party_CSV_v2/V-Dem-CPD-Party-V2.csv"
VDEM_CY_CSV  = DATASETS_DIR / "V-Dem-CY-Core_csv_v13/V-Dem-CY-Core-v13.csv"
VDEM_FO_CSV  = DATASETS_DIR / "V-Dem-CY-FullOthers_csv_v13/V-Dem-CY-Full+Others-v13.csv"
PARLGOV_PARTIES = DATASETS_DIR / "parlgov2024.csv"
PARLGOV_CABINET = DATASETS_DIR / "parlgov_cabinet.csv"
PARTYFACTS_CSV  = DATASETS_DIR / "partyfacts-external-parties2025.csv"
MANIFESTO_CSV   = DATASETS_DIR / "MPDataset_MPDS2023a.csv"

OUT_DIR.mkdir(parents=True, exist_ok=True)

# Topic mapping
# 9-category scheme (validated κ=0.840 vs human gold, gpt-4.1-mini)
# Economy = Economic + Welfare merged
# Cultural Politics = Identity + Nationalism merged
# Law & Order = Crime + Security merged
TOPIC_CATEGORIES_9 = [
    "Cultural Politics", "Democracy", "Economy", "Education",
    "Environment", "Law & Order", "Migration", "Other", "Technology",
]
# Keep TOPIC_LABELS as identity map for compatibility with downstream column naming
TOPIC_LABELS = {c: c for c in TOPIC_CATEGORIES_9}
TOPIC_COLS = [f"n_topic_{v.lower().replace(' ', '_').replace('&', 'and')}"
              for v in TOPIC_CATEGORIES_9]

# Country name harmonisation (V-Dem → our panel names)
VDEM_COUNTRY_MAP = {
    "United States of America": "United States",
    "United Kingdom":           "United Kingdom",
    "New Zealand":              "New Zealand",
    "Netherlands":              "Netherlands",
    "Türkiye":                  "Turkey",
    "Switzerland":              "Switzerland",
}


# 1. Load reference tables
def load_vparty() -> pd.DataFrame:
    """
    Load V-Party, keep the most recent available year per party
    (carries 2019 values forward to 2020-2022 for parties present in 2019).
    Key: pf_party_id (= partyfactsid in our panel).
    """
    print("Loading V-Party …")
    vp = pd.read_csv(VPARTY_CSV, low_memory=False)
    vp = vp[vp["pf_party_id"].notna()].copy()
    vp["pf_party_id"] = pd.to_numeric(vp["pf_party_id"], errors="coerce")
    vp = vp.dropna(subset=["pf_party_id"])
    vp["pf_party_id"] = vp["pf_party_id"].astype("int64")

    # Keep the latest observation per party (covers 2019 for most)
    keep_cols = [
        "pf_party_id",
        "v2xpa_popul",      # populism index
        "v2papariah",       # pariahness
        "v2paanteli",       # anti-elite salience (component)
        "v2papeople",       # people-centrism
        "v2pariglef",       # economic left-right
        "v2padisa",         # internal party disagreement (elite)
        "v2paind",          # party personalization (leader centrism)
        "ep_antielite_salience",   # EP survey (very sparse)
        "ep_v9_popul_saliency",    # EP survey populism
    ]
    keep_cols = [c for c in keep_cols if c in vp.columns]
    vp = vp[keep_cols + ["year"]].sort_values("year")
    # Latest per party (carry-forward strategy)
    vp_latest = vp.drop(columns="year").groupby("pf_party_id").last().reset_index()

    print(f"  V-Party: {len(vp_latest)} parties")
    cov = vp_latest["v2xpa_popul"].notna().sum()
    print(f"  v2xpa_popul non-null: {cov} / {len(vp_latest)} ({cov/len(vp_latest):.0%})")
    return vp_latest


def load_parlgov() -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Returns:
      pg_parties: party_id → left_right (+ partyfactsid via PartFacts link)
      pg_cabinet: programmatic cabinet membership (party_id, start_date, end_date)
    """
    print("Loading ParlGov …")
    # Party table
    pg = pd.read_csv(PARLGOV_PARTIES)
    pg = pg[["party_id", "party_name_english", "country_name", "left_right", "family_id"]].copy()

    # Link ParlGov party_id → partyfactsid via PartFacts
    pf = pd.read_csv(PARTYFACTS_CSV)
    pf_pg = (
        pf[pf["dataset_key"] == "parlgov"][["partyfacts_id", "dataset_party_id"]]
        .dropna()
        .assign(
            partyfacts_id=lambda d: pd.to_numeric(d["partyfacts_id"], errors="coerce"),
            dataset_party_id=lambda d: pd.to_numeric(d["dataset_party_id"], errors="coerce"),
        )
        .dropna()
        .astype({"partyfacts_id": "int64", "dataset_party_id": "int64"})
        .rename(columns={"dataset_party_id": "party_id"})
    )
    pg = pg.merge(pf_pg, on="party_id", how="left")
    print(f"  ParlGov parties: {len(pg)}, with partyfacts link: {pg['partyfacts_id'].notna().sum()}")

    # Cabinet table — build party × period lookup
    cab = pd.read_csv(PARLGOV_CABINET, parse_dates=["start_date"])
    cab = cab[cab["cabinet_party"] == 1][["party_id", "start_date", "previous_cabinet_id"]].copy()

    # Build end_date: start_date of the next cabinet for same country
    cab_full = pd.read_csv(PARLGOV_CABINET, parse_dates=["start_date"])
    # Cabinet start → end mapping
    cab_starts = (
        cab_full[["cabinet_id", "start_date"]]
        .drop_duplicates()
        .rename(columns={"cabinet_id": "previous_cabinet_id", "start_date": "end_date"})
    )
    cab = cab.merge(cab_starts, on="previous_cabinet_id", how="left")
    # Fill missing end_date with far future
    cab["end_date"] = cab["end_date"].fillna(pd.Timestamp("2030-01-01"))
    cab = cab[["party_id", "start_date", "end_date"]].drop_duplicates()
    print(f"  Cabinet periods: {len(cab)}")

    return pg, cab


def load_vdem_country() -> pd.DataFrame:
    """
    V-Dem country-year: core indices + social media vars (from FullOthers).
    Returns country × year table.
    """
    print("Loading V-Dem country-year …")
    core_cols = ["country_name", "year", "v2x_libdem", "v2x_polyarchy",
                 "v2x_freexp_altinf", "v2x_egaldem"]
    core = pd.read_csv(VDEM_CY_CSV, usecols=core_cols, low_memory=False)

    sm_cols = ["country_name", "year", "v2smpolsoc", "v2smcamp", "v2smpolhate", "v2smgovfilprc"]
    fo = pd.read_csv(VDEM_FO_CSV, usecols=sm_cols, low_memory=False)

    vd = core.merge(fo, on=["country_name", "year"], how="left")
    # Harmonise country names
    vd["country_name"] = vd["country_name"].replace(VDEM_COUNTRY_MAP)
    print(f"  V-Dem: {len(vd)} country-years, {vd['country_name'].nunique()} countries")
    return vd


def load_topics() -> None:
    """Topics are now embedded as topic_v2 in each chunk — no pre-loading needed."""
    print("Topics: reading topic_v2 column inline from each chunk (9-cat scheme).")
    return None


# 2. Build cabinet flag from ParlGov (programmatic)
def assign_parlgov_cabinet(df: pd.DataFrame, pg_parties: pd.DataFrame,
                            pg_cabinet: pd.DataFrame) -> pd.Series:
    """
    For each row in df, check if its parlgov party_id was in government
    during the tweet's month. Returns a 0/1 Series aligned to df.index.
    """
    # Build lookup: parlgovid → list of (start, end)
    cab_lookup: dict[int, list[tuple]] = {}
    for _, row in pg_cabinet.iterrows():
        pid = row["party_id"]
        if pid not in cab_lookup:
            cab_lookup[pid] = []
        cab_lookup[pid].append((row["start_date"], row["end_date"]))

    def _in_gov(row):
        pid = row.get("parlgovid")
        dt  = row.get("_dt")
        if pd.isna(pid) or dt is None:
            return np.nan
        pid = int(pid)
        periods = cab_lookup.get(pid, [])
        for s, e in periods:
            if s <= dt < e:
                return 1
        return 0

    # Only recalculate for rows where parlgov id is available
    df2 = df[["parlgovid", "_dt"]].copy()
    result = df2.apply(_in_gov, axis=1)
    return result


# 3. Load and aggregate tweet chunks
def load_and_aggregate(
    topic_map: pd.Series,
    min_tweets_cell: int = 1,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Iterates over all parquet chunks; for each tweet:
      - Assigns topic
      - Aggregates to (country, partyfactsid, month) with:
        * n_tweets, y_any, y_strong
        * n_tweets per topic (wide, for the main panel)
        * time-varying within vars (cabinet, election, covid) as FRACTIONS
      - Also aggregates long-format (country, partyfactsid, month, topic) with
        actual y_any/y_strong counts per topic (for topic incivility panel).
    Returns (party_panel, country_panel, topic_incivility_raw).
    """
    chunks = sorted(CHUNKS_DIR.glob("m_slim_*.parquet"))
    print(f"Loading {len(chunks)} parquet chunks …")

    agg_rows = []   # list of per-chunk aggregation DataFrames
    country_rows = []
    topic_inc_rows = []  # long-format topic × incivility aggregation

    TOPIC_UNIQUE = TOPIC_CATEGORIES_9  # ordered 9-cat list

    for chunk_path in tqdm(chunks, desc="Aggregating chunks"):
        df = pd.read_parquet(chunk_path)

        # Basic filters
        df = df[df["incivility_llm_v2"].notna()].copy()
        df = df[df["partyfactsid"].notna()].copy()
        df = df[df["country"].notna()].copy()
        df["created_at"] = pd.to_datetime(df["created_at"], errors="coerce")
        df = df[df["created_at"].notna()].copy()

        # Month key
        df["month"] = df["created_at"].dt.to_period("M").dt.to_timestamp()

        # Incivility indicators
        df["y_any"]    = (df["incivility_llm_v2"].astype(float) >= 1).astype("int8")
        df["y_strong"] = (df["incivility_llm_v2"].astype(float) >= 2).astype("int8")

        # Topic
        # topic_v2 is already in the chunk (9-cat scheme, gpt-4.1-mini)
        df["topic"] = df["topic_v2"] if "topic_v2" in df.columns else np.nan
        # Clamp to valid categories (drop any stray values)
        df.loc[~df["topic"].isin(TOPIC_CATEGORIES_9), "topic"] = np.nan

        # Cabinet flag: use existing column if present,
        #    but recompute for rows that are missing (manual fill-in already done) ─
        if "cabinet_cabinet_party" not in df.columns:
            df["cabinet_cabinet_party"] = np.nan

        # Core aggregation by country × party × month
        grp = df.groupby(["country", "partyfactsid", "month"], sort=False)
        core = grp.agg(
            n_tweets=("y_any", "size"),
            y_any=("y_any", "sum"),
            y_strong=("y_strong", "sum"),
            # Within vars as fractions
            frac_cabinet=("cabinet_cabinet_party", "mean"),
            frac_election=("is_election", "mean"),
            frac_covid_shock=("during_covid_chock", "mean"),
            frac_after_covid=("after_covid", "mean"),
            # Covariate means (will be averaged across chunks; weighted later)
            parlgovid_mode=("parlgovid", lambda x: x.dropna().mode().iloc[0]
                            if x.notna().any() else np.nan),
            party_mf_parfam=("party_mf_parfam", lambda x: x.dropna().mode().iloc[0]
                             if x.notna().any() else np.nan),
        ).reset_index()

        # Topic breakdown
        has_topic = df["topic"].notna()
        if has_topic.any():
            topic_grp = (
                df[has_topic]
                .groupby(["country", "partyfactsid", "month", "topic"], sort=False)
                .size()
                .unstack("topic", fill_value=0)
                .reset_index()
            )
            # Rename columns
            topic_grp = topic_grp.rename(
                columns={t: f"n_topic_{t.lower().replace(' ', '_').replace('&', 'and')}"
                         for t in TOPIC_UNIQUE if t in topic_grp.columns}
            )
            core = core.merge(topic_grp, on=["country", "partyfactsid", "month"], how="left")

            # Long-format topic incivility (actual counts per topic)
            topic_inc = (
                df[has_topic]
                .groupby(["country", "partyfactsid", "month", "topic"], sort=False)
                .agg(
                    n_tweets=("y_any", "size"),
                    y_any=("y_any", "sum"),
                    y_strong=("y_strong", "sum"),
                )
                .reset_index()
            )
            topic_inc_rows.append(topic_inc)

        agg_rows.append(core)

        # Country × month (for country-level descriptives)
        ct_core = (
            df.groupby(["country", "month"], sort=False)
            .agg(
                n_tweets=("y_any", "size"),
                y_any=("y_any", "sum"),
                y_strong=("y_strong", "sum"),
            )
            .reset_index()
        )
        country_rows.append(ct_core)

    print("Merging chunks …")
    party = pd.concat(agg_rows, ignore_index=True)
    # Sum across chunks for the same cell
    num_cols  = ["n_tweets", "y_any", "y_strong"] + \
                [c for c in party.columns if c.startswith("n_topic_")]
    frac_cols = ["frac_cabinet", "frac_election", "frac_covid_shock", "frac_after_covid"]

    # For fractional cols: weighted mean (weight = n_tweets)
    party_final = (
        party.groupby(["country", "partyfactsid", "month"], sort=True)
        .apply(_weighted_agg, num_cols=num_cols, frac_cols=frac_cols)
        .reset_index()
    )

    ct = pd.concat(country_rows, ignore_index=True)
    ct_final = (
        ct.groupby(["country", "month"], sort=True)
        [["n_tweets", "y_any", "y_strong"]]
        .sum()
        .reset_index()
    )

    if topic_inc_rows:
        topic_inc_all = pd.concat(topic_inc_rows, ignore_index=True)
        topic_inc_final = (
            topic_inc_all
            .groupby(["country", "partyfactsid", "month", "topic"], sort=True)
            [["n_tweets", "y_any", "y_strong"]]
            .sum()
            .reset_index()
        )
    else:
        topic_inc_final = pd.DataFrame()

    return party_final, ct_final, topic_inc_final


def _weighted_agg(g: pd.DataFrame, num_cols: list, frac_cols: list) -> pd.Series:
    """Aggregate a group: sum integers, weighted-mean fractions."""
    out = {}
    n = g["n_tweets"].sum()
    for c in num_cols:
        if c in g.columns:
            out[c] = g[c].sum()
    for c in frac_cols:
        if c in g.columns:
            out[c] = np.average(g[c].fillna(0), weights=g["n_tweets"])
    # parlgovid and party_mf_parfam: mode across chunks
    if "parlgovid_mode" in g.columns:
        modes = g["parlgovid_mode"].dropna()
        out["parlgovid"] = modes.mode().iloc[0] if len(modes) > 0 else np.nan
    if "party_mf_parfam" in g.columns:
        modes = g["party_mf_parfam"].dropna()
        out["party_mf_parfam"] = modes.mode().iloc[0] if len(modes) > 0 else np.nan
    return pd.Series(out)


# 4. Merge covariates onto party panel
def merge_vparty(panel: pd.DataFrame, vp: pd.DataFrame) -> pd.DataFrame:
    """Merge V-Party by partyfactsid (carry-forward already applied in load_vparty)."""
    panel["partyfactsid"] = pd.to_numeric(panel["partyfactsid"], errors="coerce")
    vp2 = vp.rename(columns={"pf_party_id": "partyfactsid"})
    out = panel.merge(vp2, on="partyfactsid", how="left")
    cov = out["v2xpa_popul"].notna().mean()
    print(f"  After V-Party merge: v2xpa_popul coverage = {cov:.1%}")
    return out


def fill_missing_party_leftright(panel: pd.DataFrame, pg: pd.DataFrame) -> pd.DataFrame:
    """
    For parties missing v2pariglef (V-Party L/R), impute from ParlGov left_right.
    ParlGov scale 0–10; V-Party v2pariglef is 0–6.
    We rescale: pg_lr_rescaled = pg_left_right * 6/10 - 0   (simple linear).
    """
    pg2 = pg[["partyfacts_id", "left_right"]].dropna().rename(
        columns={"partyfacts_id": "partyfactsid", "left_right": "pg_left_right"}
    )
    pg2["partyfactsid"] = pd.to_numeric(pg2["partyfactsid"], errors="coerce")
    pg2 = pg2.dropna(subset=["partyfactsid"]).astype({"partyfactsid": "int64"})

    panel = panel.merge(pg2, on="partyfactsid", how="left")

    # Fill v2pariglef where missing with rescaled ParlGov L/R
    miss = panel["v2pariglef"].isna() & panel["pg_left_right"].notna()
    panel.loc[miss, "v2pariglef"] = panel.loc[miss, "pg_left_right"] * (6.0 / 10.0)
    panel.loc[miss, "v2pariglef_imputed"] = 1
    panel["v2pariglef_imputed"] = panel.get("v2pariglef_imputed", pd.Series(0, index=panel.index)).fillna(0).astype("int8")

    n_filled = miss.sum()
    print(f"  Filled v2pariglef with ParlGov rescaled for {n_filled:,} rows "
          f"({n_filled/len(panel):.1%}); "
          f"remaining missing = {panel['v2pariglef'].isna().mean():.1%}")
    return panel.drop(columns="pg_left_right", errors="ignore")


def merge_vdem_country(panel: pd.DataFrame, ct_panel: pd.DataFrame,
                        vd: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Merge V-Dem country-year onto panels.
    Uses the year of each month (carry-forward for 2022 from 2021 if needed).
    """
    panel["year"] = pd.to_datetime(panel["month"]).dt.year
    ct_panel["year"] = pd.to_datetime(ct_panel["month"]).dt.year

    # Select cols
    vd_cols = ["country_name", "year", "v2x_libdem", "v2x_polyarchy",
               "v2x_freexp_altinf", "v2smpolsoc", "v2smcamp", "v2smpolhate",
               "v2smgovfilprc", "v2x_egaldem"]
    vd_cols = [c for c in vd_cols if c in vd.columns]
    vd_sub = vd[vd_cols].copy().rename(columns={"country_name": "country"})

    # Carry forward: if 2022 data missing, use 2021
    vd_max_yr = vd_sub.groupby("country")["year"].max()
    years_needed = set(panel["year"].unique())
    extras = []
    for yr in sorted(years_needed):
        for country, max_yr in vd_max_yr.items():
            if yr > max_yr:
                row = vd_sub[(vd_sub["country"] == country) & (vd_sub["year"] == max_yr)]
                if len(row):
                    r = row.copy()
                    r["year"] = yr
                    extras.append(r)
    if extras:
        vd_sub = pd.concat([vd_sub] + extras, ignore_index=True)

    panel    = panel.merge(vd_sub, on=["country", "year"], how="left")
    ct_panel = ct_panel.merge(vd_sub, on=["country", "year"], how="left")

    cov = panel["v2x_libdem"].notna().mean()
    print(f"  V-Dem libdem coverage: {cov:.1%}")
    return panel, ct_panel


def add_parlgov_cabinet_programmatic(panel: pd.DataFrame, pg_cabinet: pd.DataFrame,
                                      pg_parties: pd.DataFrame) -> pd.DataFrame:
    """
    Add/overwrite frac_cabinet using ParlGov cabinet data.
    For parties with parlgov ids, assign 1 if in cabinet during the month,
    then average within cell (since frac_cabinet is already a fraction).
    This is the authoritative version; complements the manual fixes.
    """
    # Build parlgov_id → partyfactsid map
    pg2 = pg_parties[["party_id", "partyfacts_id"]].dropna().copy()
    pg2["partyfacts_id"] = pd.to_numeric(pg2["partyfacts_id"], errors="coerce")
    pg2 = pg2.dropna().astype({"party_id": "int64", "partyfacts_id": "int64"})
    pf_to_pg = pg2.set_index("partyfacts_id")["party_id"].to_dict()

    # Build cabinet period set: parlgov_id → [(start, end)]
    cab_lookup: dict[int, list] = {}
    for _, row in pg_cabinet.iterrows():
        pid = int(row["party_id"])
        cab_lookup.setdefault(pid, []).append((row["start_date"], row["end_date"]))

    def _cabinet_flag(row):
        pf = row["partyfactsid"]
        if pd.isna(pf):
            return np.nan
        pg_id = pf_to_pg.get(int(pf))
        if pg_id is None:
            return np.nan  # no ParlGov link → keep manual value
        dt = pd.Timestamp(row["month"])
        for s, e in cab_lookup.get(pg_id, []):
            if s <= dt < e:
                return 1.0
        return 0.0

    panel["_dt"] = pd.to_datetime(panel["month"])
    pg_cab = panel.apply(_cabinet_flag, axis=1)

    # Where ParlGov gives an answer, use it; otherwise keep existing frac_cabinet
    has_pg = pg_cab.notna()
    panel.loc[has_pg, "frac_cabinet"] = pg_cab[has_pg]
    panel = panel.drop(columns="_dt", errors="ignore")
    print(f"  ParlGov cabinet assigned for {has_pg.sum():,} / {len(panel):,} rows")
    return panel


def add_derived_variables(panel: pd.DataFrame) -> pd.DataFrame:
    """
    Add standardised versions of all continuous predictors,
    time indices, and party-family labels.
    """
    panel = panel.copy()
    panel["month"] = pd.to_datetime(panel["month"])
    panel = panel.sort_values(["country", "partyfactsid", "month"]).reset_index(drop=True)

    # Time
    t_min = panel["month"].dt.to_period("M").astype("int64").min()
    panel["t"] = panel["month"].dt.to_period("M").astype("int64")
    panel["t_norm"] = panel["t"] - t_min
    t_std = panel["t_norm"].std(ddof=0)
    panel["t_norm_z"] = (panel["t_norm"] - panel["t_norm"].mean()) / (t_std + 1e-12)

    # Share DVs
    panel["share_any"]    = panel["y_any"]    / panel["n_tweets"].clip(lower=1)
    panel["share_strong"] = panel["y_strong"] / panel["n_tweets"].clip(lower=1)

    # Z-score party variables
    party_z_vars = {
        "v2xpa_popul":    "party_populism_z",
        "v2pariglef":     "party_lr_z",
        "v2paind":        "party_personalization_z",
        "v2papariah":     "party_pariahness_z",
        "v2padisa":       "party_elite_disagree_z",
        "v2paanteli":     "party_antielite_z",
        "v2papeople":     "party_peoplecentrism_z",
    }
    for src, dst in party_z_vars.items():
        if src in panel.columns:
            x = panel[src]
            mu, sd = x.mean(skipna=True), x.std(skipna=True, ddof=0)
            panel[dst] = (x - mu) / (sd + 1e-12)

    # Z-score country variables
    country_z_vars = {
        "v2x_libdem":         "libdem_z",
        "v2x_freexp_altinf":  "freexp_z",
        "v2smpolsoc":         "sm_polsoc_z",
        "v2smpolhate":        "sm_polhate_z",
        "v2smcamp":           "sm_camp_z",
        "v2smgovfilprc":      "sm_govfilter_z",
    }
    for src, dst in country_z_vars.items():
        if src in panel.columns:
            x = panel[src]
            mu, sd = x.mean(skipna=True), x.std(skipna=True, ddof=0)
            panel[dst] = (x - mu) / (sd + 1e-12)

    # Right-wing populism interaction (populism × right economic)
    if "party_populism_z" in panel.columns and "party_lr_z" in panel.columns:
        raw = panel["party_populism_z"] * panel["party_lr_z"]
        panel["rwpop_interaction_z"] = (raw - raw.mean(skipna=True)) / (raw.std(skipna=True, ddof=0) + 1e-12)

    print(f"  Derived variables added. Shape: {panel.shape}")
    return panel


def build_topic_incivility_panel(
    topic_raw: pd.DataFrame, panel: pd.DataFrame
) -> pd.DataFrame:
    """
    Long-format panel: party × country × month × topic.
    n_tweets / y_any / y_strong are actual counts for tweets classified into
    that topic (not a proportional-allocation proxy).
    Covariates are joined from the filtered main panel (≥10 tweets/cell),
    so only cells present in the main panel are retained.
    """
    if topic_raw.empty:
        print("  [skip] no topic incivility data")
        return pd.DataFrame()

    skip_cols = {"n_tweets", "y_any", "y_strong", "share_any", "share_strong"}
    skip_cols |= {c for c in panel.columns if c.startswith("n_topic_")}
    covariate_cols = [c for c in panel.columns if c not in skip_cols]
    join_cols = ["country", "partyfactsid", "month"] + [
        c for c in covariate_cols if c not in {"country", "partyfactsid", "month"}
    ]

    tp = topic_raw.merge(panel[join_cols], on=["country", "partyfactsid", "month"], how="inner")
    tp["share_any"]    = tp["y_any"]    / tp["n_tweets"].clip(lower=1)
    tp["share_strong"] = tp["y_strong"] / tp["n_tweets"].clip(lower=1)
    tp = tp.sort_values(["country", "partyfactsid", "month", "topic"]).reset_index(drop=True)

    print(f"  Topic incivility panel: {len(tp):,} rows, "
          f"{tp['topic'].nunique()} topics, "
          f"{tp[['country','partyfactsid','month']].drop_duplicates().shape[0]:,} cells")
    return tp


def add_country_panel_covariates(ct: pd.DataFrame, party: pd.DataFrame) -> pd.DataFrame:
    """
    Merge country-level covariates (from party panel country means) onto country panel.
    Also compute share_election / share_cabinet aggregated over parties.
    """
    # V-Dem country vars: average across parties for same country-month (they're country-level)
    cy_vars = ["v2x_libdem","v2x_freexp_altinf","v2smpolsoc","v2smcamp",
               "v2smpolhate","v2smgovfilprc","v2x_polyarchy","v2x_egaldem"]
    cy_vars = [v for v in cy_vars if v in party.columns]

    cy_agg = (
        party.groupby(["country","month"])[cy_vars]
        .first()
        .reset_index()
    )

    # Aggregated within vars (tweet-weighted)
    within_agg = (
        party.assign(
            w_cabinet  = lambda d: d["frac_cabinet"]    * d["n_tweets"],
            w_election = lambda d: d["frac_election"]   * d["n_tweets"],
        )
        .groupby(["country","month"])
        .agg(
            sum_w_cabinet  = ("w_cabinet",  "sum"),
            sum_w_election = ("w_election", "sum"),
            total_tweets   = ("n_tweets",   "sum"),
        )
        .assign(
            share_cabinet  = lambda d: d["sum_w_cabinet"]  / d["total_tweets"].clip(lower=1),
            share_election = lambda d: d["sum_w_election"] / d["total_tweets"].clip(lower=1),
        )[["share_cabinet","share_election"]]
        .reset_index()
    )

    ct = ct.merge(cy_agg,    on=["country","month"], how="left")
    ct = ct.merge(within_agg, on=["country","month"], how="left")
    ct["share_any"]    = ct["y_any"]    / ct["n_tweets"].clip(lower=1)
    ct["share_strong"] = ct["y_strong"] / ct["n_tweets"].clip(lower=1)

    # Time
    t_min = ct["month"].dt.to_period("M").astype("int64").min()
    ct["t"] = ct["month"].dt.to_period("M").astype("int64")
    ct["t_norm"] = ct["t"] - t_min
    return ct


# 5. Coverage report
def coverage_report(panel: pd.DataFrame) -> None:
    print("\nCoverage report (party x country x month panel):")
    print(f"  Total rows: {len(panel):,}")
    print(f"  Countries:  {panel['country'].nunique()}")
    print(f"  Parties:    {panel['partyfactsid'].nunique()}")
    print(f"  Date range: {panel['month'].min().date()} → {panel['month'].max().date()}")
    print(f"  Total tweets: {panel['n_tweets'].sum():,.0f}")
    print(f"  Overall share_strong: {(panel['y_strong'].sum()/panel['n_tweets'].sum()):.3f}")
    print()

    vars_to_check = {
        "v2xpa_popul":           "Populism index (V-Party)",
        "v2pariglef":            "Econ left-right (V-Party + ParlGov fill)",
        "v2paind":               "Personalization (V-Party)",
        "v2papariah":            "Pariahness (V-Party)",
        "v2padisa":              "Elite disagree (V-Party)",
        "v2paanteli":            "Anti-elite (V-Party)",
        "v2x_libdem":            "Liberal democracy (V-Dem)",
        "v2x_freexp_altinf":     "Freedom of expression (V-Dem)",
        "v2smpolsoc":            "SM political activity (V-Dem)",
        "v2smpolhate":           "SM hate speech (V-Dem)",
        "v2smcamp":              "SM campaign use (V-Dem)",
        "frac_cabinet":          "Cabinet party fraction",
        "frac_election":         "Election period fraction",
        "v2pariglef_imputed":    "L/R imputed from ParlGov",
    }
    for col, label in vars_to_check.items():
        if col in panel.columns:
            nn = panel[col].notna().mean()
            n_parties = panel.loc[panel[col].notna(), "partyfactsid"].nunique()
            n_miss_parties = panel.loc[panel[col].isna(), "partyfactsid"].nunique()
            print(f"  {label}: {nn:.1%} rows ({n_parties} parties covered, {n_miss_parties} missing)")
        else:
            print(f"  {label}: NOT IN PANEL")


# 6. Save outputs
def save_panels(
    panel: pd.DataFrame,
    ct: pd.DataFrame,
    topic_incivility: pd.DataFrame | None = None,
) -> None:
    print("\nSaving panels …")

    # Main party panel
    panel.to_pickle(OUT_DIR / "party_country_month_panel_v4.df.pkl")
    panel.to_parquet(OUT_DIR / "party_country_month_panel_v4.parquet", index=False)
    print(f"  Saved party panel: {panel.shape}")

    # Topic subpanel (only rows with at least 1 topic-labelled tweet) — kept for back-compat
    topic_cols = [c for c in panel.columns if c.startswith("n_topic_")]
    if topic_cols:
        has_topics = panel[topic_cols].sum(axis=1) > 0
        topic_panel = panel[has_topics].copy()
        topic_panel.to_pickle(OUT_DIR / "party_country_month_topic_panel_v4.pkl")
        print(f"  Saved topic panel: {topic_panel.shape}")

    # Long-format topic incivility panel
    if topic_incivility is not None and not topic_incivility.empty:
        topic_incivility.to_pickle(OUT_DIR / "party_country_month_topic_incivility_v4.pkl")
        topic_incivility.to_parquet(OUT_DIR / "party_country_month_topic_incivility_v4.parquet", index=False)
        print(f"  Saved topic incivility panel: {topic_incivility.shape}")

    # Country panel
    ct.to_csv(OUT_DIR / "country_month_panel_v4.csv", index=False)
    print(f"  Saved country panel: {ct.shape}")

    # Stata format
    try:
        panel_stata = panel.copy()
        for col in panel_stata.select_dtypes("period").columns:
            panel_stata[col] = panel_stata[col].astype(str)
        panel_stata.to_stata(OUT_DIR / "party_country_month_panel_v4.dta", write_index=False)
        print("  Saved .dta")
    except Exception as e:
        print(f"  .dta save warning: {e}")


# MAIN
def main():
    print("Building panels …")

    # Load reference tables
    vp         = load_vparty()
    pg, pg_cab = load_parlgov()
    vd         = load_vdem_country()
    topic_map  = load_topics()

    # Aggregate tweets
    party, ct, topic_inc_raw = load_and_aggregate(topic_map)

    # Merge party-level covariates
    party = merge_vparty(party, vp)
    party = fill_missing_party_leftright(party, pg)
    party = add_parlgov_cabinet_programmatic(party, pg_cab, pg)

    # Merge country-level covariates (V-Dem)
    party, ct = merge_vdem_country(party, ct, vd)

    # Derived variables
    party = add_derived_variables(party)
    ct    = add_country_panel_covariates(ct, party)

    # Filter to cells with enough tweets
    panel_10 = party[party["n_tweets"] >= 10].copy()

    # Long-format topic incivility panel (covariates joined from filtered main panel)
    topic_incivility = build_topic_incivility_panel(topic_inc_raw, panel_10)

    # Report
    coverage_report(panel_10)

    # Save
    save_panels(panel_10, ct, topic_incivility)

    print("\nDone.")
    return panel_10, ct, topic_incivility


if __name__ == "__main__":
    panel, ct, topic_incivility = main()
