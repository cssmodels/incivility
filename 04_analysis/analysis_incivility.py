"""
Main analysis for the political incivility paper: a hierarchical Bayesian
beta-binomial model of y_strong (tweets scored >= 2, "very uncivil") out of
n_tweets per country x party x month cell.

The model has a global linear time trend, country random intercepts with
correlated random slopes for time (LKJ prior), i.i.d. month shocks, within-
observation covariates (cabinet, election, COVID), party-level z-scored
predictors (populism, economic left-right, right-wing-populism interaction),
and a country-level z-scored predictor (liberal democracy index).

    python analysis_incivility.py [--data PATH] [--outdir PATH] [--chains 4] [--draws 2000]

Writes idata_main.nc / idata_partyfamily.nc (posterior samples), effects_summary.csv
(odds ratios + delta-p table), and fig_*.pdf to --outdir.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path

import arviz as az
import matplotlib as mpl
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pymc as pm

# Limit CPU thread noise on shared nodes
for _k in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ.setdefault(_k, "1")


# Helpers
def safe_zscore(s: pd.Series) -> pd.Series:
    x = pd.to_numeric(s, errors="coerce")
    mu, sd = x.mean(skipna=True), x.std(skipna=True, ddof=0)
    if sd is None or np.isnan(sd) or sd < 1e-12:
        return pd.Series(np.zeros(len(x)), index=x.index, dtype="float64")
    return ((x - mu) / sd).astype("float64")


def flat(da):
    """Flatten xarray DataArray → (sample, ...) regardless of stacking state."""
    if "chain" in da.dims and "draw" in da.dims:
        return da.stack(sample=("chain", "draw")).transpose("sample", ...).values
    if "sample" in da.dims:
        return da.transpose("sample", ...).values
    return da.values


def stacked_posterior(idata: az.InferenceData):
    """Return posterior dataset with a flat 'sample' dim (handles both fresh and loaded idata)."""
    p = idata.posterior
    if "chain" in p.dims and "draw" in p.dims:
        return p.stack(sample=("chain", "draw"))
    return p


def hdi95(x: np.ndarray) -> tuple[float, float]:
    return float(np.quantile(x, 0.025)), float(np.quantile(x, 0.975))


def logistic(x):
    return 1.0 / (1.0 + np.exp(-x))


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
        "xtick.major.width": 0.7,
        "ytick.major.width": 0.7,
        "xtick.major.size": 3.0,
        "ytick.major.size": 3.0,
        "lines.linewidth": 0.9,
        "lines.markersize": 4.5,
    })


SINGLE_W = 3.35   # ~85 mm
DOUBLE_W = 7.10   # ~180 mm


def save_fig(fig: plt.Figure, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path)
    plt.close(fig)
    print(f"  saved → {path}")


# Data preparation
WITHIN_VARS = ["frac_cabinet", "frac_election", "frac_covid_shock", "frac_after_covid"]
PARTY_BASE_VARS = ["v2xpa_popul", "v2pariglef"]
COUNTRY_VARS = ["v2x_libdem"]

PARFAM_LABELS = {
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
    "DIV": "Diverse/Other",
}


def _make_libdem_dev_z(df: pd.DataFrame) -> np.ndarray:
    """
    Within-country deviation of libdem from the country mean, z-scored across all obs.
    Captures democratic backsliding / improvement within country over time.
    """
    country_mean = df.groupby("country")["v2x_libdem"].transform("mean")
    dev = pd.to_numeric(df["v2x_libdem"], errors="coerce") - country_mean
    dev = dev.fillna(0.0)
    sd = dev.std(ddof=0)
    if sd < 1e-12:
        return np.zeros(len(df), dtype=np.float64)
    return ((dev - dev.mean()) / sd).to_numpy(np.float64)


def prepare_data(pt: pd.DataFrame, min_tweets: int = 5, dv: str = "y_strong") -> dict:
    """
    Prepare model data from the party × country × month panel.

    Parameters
    ----------
    pt:          Raw panel (party_country_month_panel.df.pkl)
    min_tweets:  Drop cells with fewer than this many tweets
    dv:          'y_strong' (recommended) or 'y_any'

    Returns
    -------
    dict with all arrays and metadata needed by fit_main_model / fit_partyfamily_model
    """
    assert dv in {"y_strong", "y_any"}, f"dv must be 'y_strong' or 'y_any', got {dv!r}"

    df = pt.copy()
    df[dv] = pd.to_numeric(df[dv], errors="coerce")
    df["n_tweets"] = pd.to_numeric(df["n_tweets"], errors="coerce")
    df["month"] = pd.to_datetime(df["month"], errors="coerce")

    df = df.dropna(subset=[dv, "n_tweets", "country", "partyfactsid", "month"])
    df[dv] = df[dv].astype("int64")
    df["n_tweets"] = df["n_tweets"].astype("int64")
    df = df[
        (df["n_tweets"] >= min_tweets) &
        (df[dv] >= 0) &
        (df[dv] <= df["n_tweets"])
    ].reset_index(drop=True)

    # Group indices
    df["country_idx"] = pd.Categorical(df["country"]).codes.astype("int64")
    df["party_uid"] = df["country"].astype(str) + "___" + df["partyfactsid"].astype(str)
    df["party_idx"] = pd.Categorical(df["party_uid"]).codes.astype("int64")

    country_levels = pd.Categorical(df["country"]).categories
    party_uid_levels = pd.Categorical(df["party_uid"]).categories

    n_country = int(df["country_idx"].max() + 1)
    n_party = int(df["party_idx"].max() + 1)

    # Time
    month_levels = pd.Index(sorted(df["month"].unique()))
    df["t_idx"] = df["month"].map({m: i for i, m in enumerate(month_levels)}).astype("int64")
    n_time = len(month_levels)

    t_raw = df["t_idx"].astype("float64")
    df["t_slope"] = (t_raw - t_raw.mean()) / (t_raw.std(ddof=0) + 1e-12)

    # Within-observation predictors
    for col in WITHIN_VARS:
        if col not in df.columns:
            df[col] = 0.0
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0.0).clip(0.0, 1.0).astype("float64")

    # Party-level Z matrix
    party_g = df.groupby("party_uid")[PARTY_BASE_VARS].mean(numeric_only=True).copy()
    party_g["pop_z"] = safe_zscore(party_g["v2xpa_popul"])
    party_g["lr_z"] = safe_zscore(party_g["v2pariglef"])
    party_g["pop_x_lr_z"] = safe_zscore(party_g["pop_z"] * party_g["lr_z"])

    party_term_names = ["party_populism_index_z", "party_econ_left_right_z", "populism_x_rightleft_z"]
    party_Z = (
        party_g[["pop_z", "lr_z", "pop_x_lr_z"]]
        .rename(columns={"pop_z": "party_populism_index_z",
                          "lr_z": "party_econ_left_right_z",
                          "pop_x_lr_z": "populism_x_rightleft_z"})
        .reindex(party_uid_levels)
        .fillna(0.0)
        .to_numpy(np.float64)
    )

    # Country-level Z matrix
    country_g = df.groupby("country")[COUNTRY_VARS].mean(numeric_only=True).copy()
    country_g["libdem_z"] = safe_zscore(country_g["v2x_libdem"])

    country_term_names = ["liberal_democracy_index_z"]
    country_Z = (
        country_g[["libdem_z"]]
        .rename(columns={"libdem_z": "liberal_democracy_index_z"})
        .reindex(country_levels)
        .fillna(0.0)
        .to_numpy(np.float64)
    )

    # Party-family index (for party-family model)
    pf_col = None
    for _c in ["party_mf_parfam", "family_name_short", "partyfamily"]:
        if _c in df.columns:
            pf_col = _c
            break
    pf_levels = None
    pf_idx_arr = None
    if pf_col:
        pf_cat = pd.Categorical(df[pf_col].fillna("DIV"))
        pf_levels = list(pf_cat.categories)
        df["pf_idx"] = pf_cat.codes.astype("int64")
        pf_idx_arr = df["pf_idx"].to_numpy(np.int64)

    return dict(
        df=df,
        y=df[dv].to_numpy(np.int64),
        n=df["n_tweets"].to_numpy(np.int64),
        Xw=df[WITHIN_VARS].to_numpy(np.float64),
        party_Z=party_Z,
        country_Z=country_Z,
        country_idx=df["country_idx"].to_numpy(np.int64),
        party_idx=df["party_idx"].to_numpy(np.int64),
        t_idx=df["t_idx"].to_numpy(np.int64),
        t_slope=df["t_slope"].to_numpy(np.float64),
        pf_idx=pf_idx_arr,
        n_country=n_country,
        n_party=n_party,
        n_time=n_time,
        country_levels=list(country_levels),
        party_uid_levels=list(party_uid_levels),
        month_levels=month_levels,
        party_term_names=party_term_names,
        country_term_names=country_term_names,
        pf_levels=pf_levels,
        within_vars=WITHIN_VARS,
        dv=dv,
        # Pre-extracted 1-D arrays for interaction models
        pop_z_party=party_Z[:, 0].copy(),           # populism z per party
        libdem_z_country=country_Z[:, 0].copy(),    # libdem z per country
        frac_cabinet=df["frac_cabinet"].to_numpy(np.float64),
        # Time-varying libdem deviation (within-country, for democratic backsliding model)
        libdem_dev_z=_make_libdem_dev_z(df),
    )


# Main model
def build_main_model(d: dict) -> tuple[pm.Model, dict]:
    """
    Beta-Binomial hierarchical model with:
      - Global intercept + time slope
      - Within-obs effects (election, cabinet, COVID)
      - Country correlated RE (intercept + slope) explained by liberal democracy
      - Party RE explained by populism + L/R + interaction
      - Month shocks (i.i.d., regularized)
      - Beta-Binomial overdispersion via kappa
    """
    n_obs = len(d["y"])
    coords = {
        "obs": np.arange(n_obs),
        "time": np.arange(d["n_time"]),
        "country": np.arange(d["n_country"]),
        "party": np.arange(d["n_party"]),
        "within_term": d["within_vars"],
        "party_term": d["party_term_names"],
        "country_term": d["country_term_names"],
        "re_coef": ["intercept", "time_slope"],
    }

    with pm.Model(coords=coords) as model:
        # Data containers
        y_d   = pm.Data("y",          d["y"],          dims="obs")
        n_d   = pm.Data("n",          d["n"],          dims="obs")
        Xw_d  = pm.Data("Xw",         d["Xw"],         dims=("obs", "within_term"))
        pZ_d  = pm.Data("party_Z",    d["party_Z"],    dims=("party", "party_term"))
        cZ_d  = pm.Data("country_Z",  d["country_Z"],  dims=("country", "country_term"))
        ci_d  = pm.Data("country_idx", d["country_idx"], dims="obs")
        pi_d  = pm.Data("party_idx",  d["party_idx"],  dims="obs")
        ti_d  = pm.Data("t_idx",      d["t_idx"],      dims="obs")
        ts_d  = pm.Data("t_slope",    d["t_slope"],    dims="obs")

        # Population-level priors
        alpha  = pm.Normal("alpha",    0.0, 1.2)
        beta_t = pm.Normal("beta_t",   0.0, 0.20)
        beta_w = pm.Normal("beta_within", 0.0, 0.25, dims="within_term")

        # Month shocks
        sigma_time  = pm.Exponential("sigma_time", 5.0)
        a_time_raw  = pm.Normal("a_time_raw", 0.0, sigma_time, dims="time")
        a_time      = pm.Deterministic(
            "a_time", a_time_raw - pm.math.mean(a_time_raw), dims="time"
        )

        # Country correlated RE (intercept + slope)
        gamma_c_int   = pm.Normal("gamma_country_int",   0.0, 0.25, dims="country_term")
        gamma_c_slope = pm.Normal("gamma_country_slope", 0.0, 0.15, dims="country_term")

        chol_c, _, _ = pm.LKJCholeskyCov(
            "country_re_chol", n=2, eta=2.0,
            sd_dist=pm.Exponential.dist(5.0), compute_corr=True,
        )
        z_c    = pm.Normal("z_country", 0.0, 1.0, dims=("country", "re_coef"))
        re_c   = pm.Deterministic(
            "re_country", pm.math.dot(z_c, chol_c.T), dims=("country", "re_coef")
        )
        u_c_int   = pm.Deterministic("u_country_int",   re_c[:, 0], dims="country")
        u_c_slope = pm.Deterministic("u_country_slope", re_c[:, 1], dims="country")

        a_country = pm.Deterministic(
            "a_country",
            pm.math.dot(cZ_d, gamma_c_int) + u_c_int,
            dims="country",
        )
        b_country = pm.Deterministic(
            "b_country",
            pm.math.dot(cZ_d, gamma_c_slope) + u_c_slope,
            dims="country",
        )

        # Party RE explained by party-level predictors
        gamma_p   = pm.Normal("gamma_party", 0.0, 0.25, dims="party_term")
        sigma_p   = pm.Exponential("sigma_party", 2.0)
        z_p       = pm.Normal("z_party", 0.0, 1.0, dims="party")
        a_party   = pm.Deterministic(
            "a_party",
            pm.math.dot(pZ_d, gamma_p) + z_p * sigma_p,
            dims="party",
        )

        # Linear predictor
        eta = (
            alpha
            + pm.math.dot(Xw_d, beta_w)
            + a_time[ti_d]
            + a_country[ci_d]
            + a_party[pi_d]
            + (beta_t + b_country[ci_d]) * ts_d
        )

        p = pm.Deterministic("p", pm.math.sigmoid(eta), dims="obs")
        p_clip = pm.math.clip(p, 1e-6, 1.0 - 1e-6)

        # Beta-Binomial likelihood
        kappa = pm.Exponential("kappa", 1.0 / 50)
        pm.BetaBinomial(
            "y_obs",
            n=n_d,
            alpha=p_clip * kappa,
            beta=(1.0 - p_clip) * kappa,
            observed=y_d,
            dims="obs",
        )

    return model, coords


def fit_main_model(d: dict, draws: int = 2000, tune: int = 2000,
                   chains: int = 4, target_accept: float = 0.99,
                   outdir: Path | None = None) -> az.InferenceData:
    model, _ = build_main_model(d)
    with model:
        idata = pm.sample(
            draws=draws, tune=tune, chains=chains,
            target_accept=target_accept, max_treedepth=12,
            random_seed=42, nuts_sampler="numpyro",
            progressbar=True, return_inferencedata=True,
        )
        # Save raw samples immediately so --load-main can resume if anything crashes after
        _raw_path = Path(outdir) / "idata_main_raw.nc" if outdir else None
        if _raw_path:
            idata.to_netcdf(str(_raw_path))
            print(f"  checkpoint → {_raw_path}")
        idata.extend(pm.sample_posterior_predictive(
            idata, var_names=["y_obs"], random_seed=42,
            return_inferencedata=True,
        ))
    return idata


# Interaction models (robustness)
def _main_model_shared(d, coords, model):
    """
    Build shared components of the main beta-binomial model inside an active `with model` block.
    Returns a dict of PyMC variables needed by interaction-specific code.
    """
    y_d   = pm.Data("y",           d["y"],           dims="obs")
    n_d   = pm.Data("n",           d["n"],           dims="obs")
    Xw_d  = pm.Data("Xw",          d["Xw"],          dims=("obs", "within_term"))
    pZ_d  = pm.Data("party_Z",     d["party_Z"],     dims=("party", "party_term"))
    cZ_d  = pm.Data("country_Z",   d["country_Z"],   dims=("country", "country_term"))
    ci_d  = pm.Data("country_idx", d["country_idx"], dims="obs")
    pi_d  = pm.Data("party_idx",   d["party_idx"],   dims="obs")
    ti_d  = pm.Data("t_idx",       d["t_idx"],       dims="obs")
    ts_d  = pm.Data("t_slope",     d["t_slope"],     dims="obs")

    alpha  = pm.Normal("alpha",       0.0, 1.2)
    beta_t = pm.Normal("beta_t",      0.0, 0.20)
    beta_w = pm.Normal("beta_within", 0.0, 0.25, dims="within_term")

    sigma_time = pm.Exponential("sigma_time", 5.0)
    a_time_raw = pm.Normal("a_time_raw", 0.0, sigma_time, dims="time")
    a_time     = pm.Deterministic("a_time", a_time_raw - pm.math.mean(a_time_raw), dims="time")

    gamma_c_int   = pm.Normal("gamma_country_int",   0.0, 0.25, dims="country_term")
    gamma_c_slope = pm.Normal("gamma_country_slope", 0.0, 0.15, dims="country_term")
    chol_c, _, _  = pm.LKJCholeskyCov("country_re_chol", n=2, eta=2.0,
                                       sd_dist=pm.Exponential.dist(5.0), compute_corr=True)
    z_c    = pm.Normal("z_country", 0.0, 1.0, dims=("country", "re_coef"))
    re_c   = pm.Deterministic("re_country", pm.math.dot(z_c, chol_c.T), dims=("country", "re_coef"))
    pm.Deterministic("u_country_int",   re_c[:, 0], dims="country")
    pm.Deterministic("u_country_slope", re_c[:, 1], dims="country")
    a_country = pm.Deterministic("a_country", pm.math.dot(cZ_d, gamma_c_int)   + re_c[:, 0], dims="country")
    b_country = pm.Deterministic("b_country", pm.math.dot(cZ_d, gamma_c_slope) + re_c[:, 1], dims="country")

    gamma_p = pm.Normal("gamma_party", 0.0, 0.25, dims="party_term")
    sigma_p = pm.Exponential("sigma_party", 2.0)
    z_p     = pm.Normal("z_party", 0.0, 1.0, dims="party")
    a_party = pm.Deterministic("a_party", pm.math.dot(pZ_d, gamma_p) + z_p * sigma_p, dims="party")

    kappa   = pm.Exponential("kappa", 1.0 / 50)

    return dict(y_d=y_d, n_d=n_d, Xw_d=Xw_d, ci_d=ci_d, pi_d=pi_d, ti_d=ti_d, ts_d=ts_d,
                alpha=alpha, beta_t=beta_t, beta_w=beta_w,
                a_time=a_time, a_country=a_country, b_country=b_country, a_party=a_party,
                kappa=kappa)


def _finalize_bb(v, extra_eta, d, coords):
    """Add interaction term, compute eta → p → BetaBinomial likelihood."""
    eta = (v["alpha"]
           + pm.math.dot(v["Xw_d"], v["beta_w"])
           + v["a_time"][v["ti_d"]]
           + v["a_country"][v["ci_d"]]
           + v["a_party"][v["pi_d"]]
           + (v["beta_t"] + v["b_country"][v["ci_d"]]) * v["ts_d"]
           + extra_eta)
    p      = pm.Deterministic("p", pm.math.sigmoid(eta), dims="obs")
    p_clip = pm.math.clip(p, 1e-6, 1.0 - 1e-6)
    pm.BetaBinomial("y_obs", n=v["n_d"],
                    alpha=p_clip * v["kappa"], beta=(1.0 - p_clip) * v["kappa"],
                    observed=v["y_d"], dims="obs")


def build_pop_opp_model(d: dict) -> tuple[pm.Model, dict]:
    """Main model + populism × opposition interaction.
    Tests whether populist parties are especially uncivil when in opposition."""
    n_obs = len(d["y"])
    coords = {"obs": np.arange(n_obs), "time": np.arange(d["n_time"]),
              "country": np.arange(d["n_country"]), "party": np.arange(d["n_party"]),
              "within_term": d["within_vars"], "party_term": d["party_term_names"],
              "country_term": d["country_term_names"], "re_coef": ["intercept", "time_slope"]}
    with pm.Model(coords=coords) as model:
        v = _main_model_shared(d, coords, model)
        pop_z_d = pm.Data("pop_z_party",  d["pop_z_party"],  dims="party")
        cab_d   = pm.Data("frac_cabinet", d["frac_cabinet"], dims="obs")
        gamma_pop_opp = pm.Normal("gamma_pop_opp", 0.0, 0.25)
        _finalize_bb(v, gamma_pop_opp * pop_z_d[v["pi_d"]] * (1.0 - cab_d), d, coords)
    return model, coords


def build_pop_libdem_model(d: dict) -> tuple[pm.Model, dict]:
    """Main model + populism × liberal democracy cross-level interaction.
    Tests whether populism-incivility link is stronger in less democratic contexts."""
    n_obs = len(d["y"])
    coords = {"obs": np.arange(n_obs), "time": np.arange(d["n_time"]),
              "country": np.arange(d["n_country"]), "party": np.arange(d["n_party"]),
              "within_term": d["within_vars"], "party_term": d["party_term_names"],
              "country_term": d["country_term_names"], "re_coef": ["intercept", "time_slope"]}
    with pm.Model(coords=coords) as model:
        v = _main_model_shared(d, coords, model)
        pop_z_d    = pm.Data("pop_z_party",      d["pop_z_party"],      dims="party")
        libdem_z_d = pm.Data("libdem_z_country", d["libdem_z_country"], dims="country")
        gamma_pop_libdem = pm.Normal("gamma_pop_libdem", 0.0, 0.25)
        _finalize_bb(v, gamma_pop_libdem * pop_z_d[v["pi_d"]] * libdem_z_d[v["ci_d"]], d, coords)
    return model, coords


def build_pop_time_model(d: dict) -> tuple[pm.Model, dict]:
    """Main model + populism × time slope interaction.
    Tests whether the populism-incivility gap widens over 2017–2022."""
    n_obs = len(d["y"])
    coords = {"obs": np.arange(n_obs), "time": np.arange(d["n_time"]),
              "country": np.arange(d["n_country"]), "party": np.arange(d["n_party"]),
              "within_term": d["within_vars"], "party_term": d["party_term_names"],
              "country_term": d["country_term_names"], "re_coef": ["intercept", "time_slope"]}
    with pm.Model(coords=coords) as model:
        v = _main_model_shared(d, coords, model)
        pop_z_d = pm.Data("pop_z_party", d["pop_z_party"], dims="party")
        gamma_pop_time = pm.Normal("gamma_pop_time", 0.0, 0.25)
        _finalize_bb(v, gamma_pop_time * pop_z_d[v["pi_d"]] * v["ts_d"], d, coords)
    return model, coords


def _fit_interaction_model(build_fn, d, draws, tune, chains, target_accept):
    model, _ = build_fn(d)
    with model:
        return pm.sample(draws=draws, tune=tune, chains=chains,
                         target_accept=target_accept, max_treedepth=12,
                         random_seed=42, nuts_sampler="numpyro",
                         progressbar=True, return_inferencedata=True)


def fit_pop_opp_model(d, draws=2000, tune=2000, chains=4, target_accept=0.99):
    return _fit_interaction_model(build_pop_opp_model, d, draws, tune, chains, target_accept)

def fit_pop_libdem_model(d, draws=2000, tune=2000, chains=4, target_accept=0.99):
    return _fit_interaction_model(build_pop_libdem_model, d, draws, tune, chains, target_accept)

def fit_pop_time_model(d, draws=2000, tune=2000, chains=4, target_accept=0.99):
    return _fit_interaction_model(build_pop_time_model, d, draws, tune, chains, target_accept)


def build_libdem_trend_model(d: dict) -> tuple[pm.Model, dict]:
    """
    Main model + time-varying libdem deviation (within-country democratic backsliding).

    libdem_dev_z = (libdem_t − country_mean_libdem) / SD_across_all_obs
    This is orthogonal to the country-mean libdem already in country_Z, so it captures
    year-on-year democratic changes within each country, not the cross-country level.

    A negative gamma_libdem_trend would mean: when democracy declines within a country,
    politicians there become more uncivil — i.e., democratic backsliding drives incivility.
    """
    n_obs = len(d["y"])
    coords = {"obs": np.arange(n_obs), "time": np.arange(d["n_time"]),
              "country": np.arange(d["n_country"]), "party": np.arange(d["n_party"]),
              "within_term": d["within_vars"], "party_term": d["party_term_names"],
              "country_term": d["country_term_names"], "re_coef": ["intercept", "time_slope"]}
    with pm.Model(coords=coords) as model:
        v = _main_model_shared(d, coords, model)
        libdem_dev_d    = pm.Data("libdem_dev_z", d["libdem_dev_z"], dims="obs")
        gamma_libdem_trend = pm.Normal("gamma_libdem_trend", 0.0, 0.25)
        _finalize_bb(v, gamma_libdem_trend * libdem_dev_d, d, coords)
    return model, coords


def fit_libdem_trend_model(d, draws=2000, tune=2000, chains=4, target_accept=0.99):
    return _fit_interaction_model(build_libdem_trend_model, d, draws, tune, chains, target_accept)


# Perspective toxicity robustness model (beta regression)
def prepare_perspective_data(pp: pd.DataFrame, min_tweets: int = 5) -> dict:
    """
    Prepare model data from the Perspective-augmented panel.
    DV is mean_toxicity_clipped (continuous 0-1), not a count.
    All covariates are identical to the main model.
    """
    df = pp.copy()
    df["mean_toxicity_clipped"] = pd.to_numeric(df["mean_toxicity_clipped"], errors="coerce")
    df["n_tweets"] = pd.to_numeric(df["n_tweets"], errors="coerce")
    df["month"]    = pd.to_datetime(df["month"], errors="coerce")

    df = df.dropna(subset=["mean_toxicity_clipped", "n_tweets", "country", "partyfactsid", "month"])
    df["n_tweets"] = df["n_tweets"].astype("int64")
    df = df[
        (df["n_tweets"] >= min_tweets) &
        (df["mean_toxicity_clipped"] > 0) &
        (df["mean_toxicity_clipped"] < 1)
    ].reset_index(drop=True)

    # Group indices
    df["country_idx"] = pd.Categorical(df["country"]).codes.astype("int64")
    df["party_uid"]   = df["country"].astype(str) + "___" + df["partyfactsid"].astype(str)
    df["party_idx"]   = pd.Categorical(df["party_uid"]).codes.astype("int64")

    country_levels   = pd.Categorical(df["country"]).categories
    party_uid_levels = pd.Categorical(df["party_uid"]).categories
    n_country = int(df["country_idx"].max() + 1)
    n_party   = int(df["party_idx"].max() + 1)

    # Time
    month_levels = pd.Index(sorted(df["month"].unique()))
    df["t_idx"]   = df["month"].map({m: i for i, m in enumerate(month_levels)}).astype("int64")
    n_time = len(month_levels)
    t_raw  = df["t_idx"].astype("float64")
    df["t_slope"] = (t_raw - t_raw.mean()) / (t_raw.std(ddof=0) + 1e-12)

    # Within-obs predictors
    for col in WITHIN_VARS:
        if col not in df.columns:
            df[col] = 0.0
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0.0).clip(0.0, 1.0).astype("float64")

    # Party-level Z matrix
    party_g = df.groupby("party_uid")[PARTY_BASE_VARS].mean(numeric_only=True).copy()
    party_g["pop_z"] = safe_zscore(party_g["v2xpa_popul"])
    party_g["lr_z"]  = safe_zscore(party_g["v2pariglef"])
    party_g["pop_x_lr_z"] = safe_zscore(party_g["pop_z"] * party_g["lr_z"])
    party_term_names = ["party_populism_index_z", "party_econ_left_right_z", "populism_x_rightleft_z"]
    party_Z = (
        party_g[["pop_z", "lr_z", "pop_x_lr_z"]]
        .rename(columns={"pop_z": "party_populism_index_z",
                         "lr_z":  "party_econ_left_right_z",
                         "pop_x_lr_z": "populism_x_rightleft_z"})
        .reindex(party_uid_levels).fillna(0.0).to_numpy(np.float64)
    )

    # Country-level Z matrix
    country_g = df.groupby("country")[COUNTRY_VARS].mean(numeric_only=True).copy()
    country_g["libdem_z"] = safe_zscore(country_g["v2x_libdem"])
    country_term_names = ["liberal_democracy_index_z"]
    country_Z = (
        country_g[["libdem_z"]]
        .rename(columns={"libdem_z": "liberal_democracy_index_z"})
        .reindex(country_levels).fillna(0.0).to_numpy(np.float64)
    )

    print(f"  Perspective panel: {len(df):,} obs, {n_country} countries, "
          f"{n_party} parties, {n_time} months")
    print(f"  mean_toxicity: median={df['mean_toxicity_clipped'].median():.4f}, "
          f"p90={df['mean_toxicity_clipped'].quantile(0.90):.4f}")

    return dict(
        df=df,
        y=df["mean_toxicity_clipped"].to_numpy(np.float64),   # continuous DV
        n=df["n_tweets"].to_numpy(np.int64),                  # kept for reference
        Xw=df[WITHIN_VARS].to_numpy(np.float64),
        party_Z=party_Z,
        country_Z=country_Z,
        country_idx=df["country_idx"].to_numpy(np.int64),
        party_idx=df["party_idx"].to_numpy(np.int64),
        t_idx=df["t_idx"].to_numpy(np.int64),
        t_slope=df["t_slope"].to_numpy(np.float64),
        n_country=n_country,
        n_party=n_party,
        n_time=n_time,
        country_levels=list(country_levels),
        party_uid_levels=list(party_uid_levels),
        month_levels=month_levels,
        party_term_names=party_term_names,
        country_term_names=country_term_names,
        within_vars=WITHIN_VARS,
        pop_z_party=party_Z[:, 0].copy(),
        libdem_z_country=country_Z[:, 0].copy(),
        frac_cabinet=df["frac_cabinet"].to_numpy(np.float64),
        libdem_dev_z=_make_libdem_dev_z(df),
        dv="mean_toxicity",
    )


def build_perspective_model(d: dict) -> tuple[pm.Model, dict]:
    """
    Beta regression on mean Perspective toxicity per cell.
    Identical structure to the main model but likelihood is Beta(mu*phi, (1-mu)*phi)
    rather than BetaBinomial — DV is a continuous mean, not a count.
    """
    n_obs = len(d["y"])
    coords = {
        "obs": np.arange(n_obs), "time": np.arange(d["n_time"]),
        "country": np.arange(d["n_country"]), "party": np.arange(d["n_party"]),
        "within_term": d["within_vars"], "party_term": d["party_term_names"],
        "country_term": d["country_term_names"], "re_coef": ["intercept", "time_slope"],
    }
    with pm.Model(coords=coords) as model:
        v = _main_model_shared(d, coords, model)

        eta = (v["alpha"]
               + pm.math.dot(v["Xw_d"], v["beta_w"])
               + v["a_time"][v["ti_d"]]
               + v["a_country"][v["ci_d"]]
               + v["a_party"][v["pi_d"]]
               + (v["beta_t"] + v["b_country"][v["ci_d"]]) * v["ts_d"])

        mu     = pm.Deterministic("mu", pm.math.sigmoid(eta), dims="obs")
        mu_c   = pm.math.clip(mu, 1e-6, 1.0 - 1e-6)

        # Beta precision (concentration); mean ~100 → SD of cell-level mean ≈ 0.005
        phi    = pm.Exponential("phi", 1.0 / 100.0)

        y_d    = pm.Data("y_tox", d["y"], dims="obs")
        pm.Beta("y_obs",
                alpha=mu_c * phi,
                beta=(1.0 - mu_c) * phi,
                observed=y_d,
                dims="obs")
    return model, coords


def fit_perspective_model(d: dict, draws: int = 2000, tune: int = 2000,
                          chains: int = 4, target_accept: float = 0.99) -> az.InferenceData:
    model, _ = build_perspective_model(d)
    with model:
        return pm.sample(
            draws=draws, tune=tune, chains=chains,
            target_accept=target_accept, max_treedepth=12,
            random_seed=42, nuts_sampler="numpyro",
            progressbar=True, return_inferencedata=True,
        )


# Party-family model
def build_partyfamily_model(d: dict) -> tuple[pm.Model, dict]:
    """
    Same as main model but replaces party-level continuous predictors with
    party-family fixed effects (hierarchically shrunk via half-Normal), and
    replaces the correlated country intercept+slope structure with simple
    independent country and month intercepts.

    The leaner country/time structure (vs. the LKJ-correlated intercept+slope
    random effects used in the main model) is needed because, unlike the main
    model, this one has no per-party random effect to absorb residual
    variance: an earlier version retained a per-party residual term on top of
    the family fixed effect, but that residual competes with the family
    effect for families with as few as 6-8 member parties, and combined with
    the LKJ country geometry produced catastrophic non-convergence
    (r_hat 3-4.5, ess_bulk ~4) — independently of a separate indexing bug
    where the family effect was looked up by the 243-level party index
    instead of the 11-level family index. This version fixes both issues.
    """
    assert d["pf_idx"] is not None, "pf_idx not set — pass a panel with 'partyfamily' column"
    n_obs = len(d["y"])
    coords = {
        "obs":         np.arange(n_obs),
        "time":        np.arange(d["n_time"]),
        "country":     np.arange(d["n_country"]),
        "partyfamily": d["pf_levels"],
        "within_term": d["within_vars"],
    }

    with pm.Model(coords=coords) as model:
        y_d  = pm.Data("y",          d["y"],           dims="obs")
        n_d  = pm.Data("n",          d["n"],           dims="obs")
        Xw_d = pm.Data("Xw",         d["Xw"],          dims=("obs", "within_term"))
        ci_d = pm.Data("country_idx", d["country_idx"], dims="obs")
        ti_d = pm.Data("t_idx",      d["t_idx"],       dims="obs")
        ts_d = pm.Data("t_slope",    d["t_slope"],     dims="obs")
        pf_d = pm.Data("pf_idx",     d["pf_idx"],      dims="obs")

        alpha  = pm.Normal("alpha",    0.0, 1.2)
        beta_t = pm.Normal("beta_t",   0.0, 0.20)
        beta_w = pm.Normal("beta_within", 0.0, 0.25, dims="within_term")

        sigma_time  = pm.Exponential("sigma_time", 5.0)
        a_time_raw  = pm.Normal("a_time_raw", 0.0, sigma_time, dims="time")
        a_time      = pm.Deterministic(
            "a_time", a_time_raw - pm.math.mean(a_time_raw), dims="time"
        )

        sigma_country  = pm.Exponential("sigma_country", 5.0)
        a_country_raw  = pm.Normal("a_country_raw", 0.0, sigma_country, dims="country")
        a_country      = pm.Deterministic(
            "a_country", a_country_raw - pm.math.mean(a_country_raw), dims="country"
        )

        # Party-family effects (hierarchically shrunk)
        sigma_pf = pm.HalfNormal("sigma_partyfamily", 0.5)
        beta_pf  = pm.Normal("beta_partyfamily", 0.0, sigma_pf, dims="partyfamily")

        eta = (
            alpha
            + pm.math.dot(Xw_d, beta_w)
            + a_time[ti_d]
            + a_country[ci_d]
            + beta_pf[pf_d]
            + beta_t * ts_d
        )

        p       = pm.Deterministic("p", pm.math.sigmoid(eta), dims="obs")
        p_clip  = pm.math.clip(p, 1e-6, 1.0 - 1e-6)
        kappa   = pm.Exponential("kappa", 1.0 / 50)

        pm.BetaBinomial(
            "y_obs",
            n=n_d,
            alpha=p_clip * kappa,
            beta=(1.0 - p_clip) * kappa,
            observed=y_d,
            dims="obs",
        )

    return model, coords


def fit_partyfamily_model(d: dict, draws: int = 2000, tune: int = 2000,
                           chains: int = 4, target_accept: float = 0.95) -> az.InferenceData:
    model, _ = build_partyfamily_model(d)
    with model:
        idata = pm.sample(
            draws=draws, tune=tune, chains=chains,
            target_accept=target_accept,
            random_seed=42, nuts_sampler="numpyro",
            progressbar=True, return_inferencedata=True,
        )
    return idata


# Effects summary table
def compute_effects_table(idata: az.InferenceData, d: dict) -> pd.DataFrame:
    """
    Compute odds ratios and Δp (in percentage points) for all effects.
    Δp computed as the change in probability from the global baseline.
    """
    post = stacked_posterior(idata)
    alpha_s = flat(post["alpha"])  # (S,)
    p0_s    = logistic(alpha_s)

    def or_and_dp(samples_1d: np.ndarray, label: str) -> dict:
        lo, hi = hdi95(samples_1d)
        or_lo, or_mid, or_hi = np.exp(lo), np.exp(samples_1d.mean()), np.exp(hi)
        dp = logistic(alpha_s + samples_1d) - p0_s
        dp_lo, dp_mid, dp_hi = np.quantile(dp, [0.025, 0.5, 0.975])
        return {
            "effect": label,
            "or_low": or_lo, "or_mid": or_mid, "or_high": or_hi,
            "dp_low_pp": dp_lo * 100, "dp_mid_pp": dp_mid * 100, "dp_high_pp": dp_hi * 100,
        }

    rows = []

    # Global time slope (beta_t)
    bt = flat(post["beta_t"])
    rows.append(or_and_dp(bt, "Time slope (global, per SD)"))

    # Within effects
    bw = flat(post["beta_within"])  # (S, K)
    for j, lab in enumerate(d["within_vars"]):
        pretty = {"frac_cabinet":     "Cabinet party",
                  "frac_election":    "Election period",
                  "frac_covid_shock": "COVID shock",
                  "frac_after_covid": "Post-COVID period"}.get(lab, lab)
        rows.append(or_and_dp(bw[:, j], pretty))

    # Party-level effects
    if "gamma_party" in post:
        gp = flat(post["gamma_party"])  # (S, P)
        for j, lab in enumerate(d["party_term_names"]):
            pretty = {"party_populism_index_z": "Populism index (+1 SD)",
                      "party_econ_left_right_z": "Econ left–right (+1 SD)",
                      "populism_x_rightleft_z": "Right-wing populism interaction (+1 SD)"}.get(lab, lab)
            rows.append(or_and_dp(gp[:, j], pretty))

    # Country-level effect
    if "gamma_country_int" in post:
        gc = flat(post["gamma_country_int"])  # (S, C)
        for j, lab in enumerate(d["country_term_names"]):
            pretty = {"liberal_democracy_index_z": "Liberal democracy (+1 SD)",
                      "sm_hate_speech_z": "SM hate speech prevalence (+1 SD)"}.get(lab, lab)
            rows.append(or_and_dp(gc[:, j], pretty))

    # Interaction terms (present only in the corresponding robustness models)
    for param, label in [
        ("gamma_pop_opp",       "Populism × opposition (+1 SD pop, full opp)"),
        ("gamma_pop_libdem",    "Populism × lib. democracy (+1 SD × +1 SD)"),
        ("gamma_pop_time",      "Populism × time (+1 SD × +1 SD)"),
        ("gamma_libdem_trend",  "Libdem within-country change (+1 SD)"),
    ]:
        if param in post:
            rows.append(or_and_dp(flat(post[param]), label))

    df_eff = pd.DataFrame(rows)
    print("\nEffects table (odds ratios + delta-p in pp):")
    print(df_eff.round(3).to_string(index=False))
    return df_eff


# Figures
def fig_forest_effects(idata: az.InferenceData, d: dict, outfile: Path) -> None:
    """Fig 1: Forest plot of key model effects on log-odds scale."""
    post = stacked_posterior(idata)

    rows: list[tuple[str, np.ndarray]] = []

    bt = flat(post["beta_t"])
    rows.append(("Time slope (global)", bt))

    bw = flat(post["beta_within"])  # (S, K)
    labels_w = {"frac_cabinet":     "Cabinet party (0→1)",
                "frac_election":    "Election period (0→1)",
                "frac_covid_shock": "COVID shock (0→1)",
                "frac_after_covid": "Post-COVID period (0→1)"}
    for j, lab in enumerate(d["within_vars"]):
        rows.append((labels_w.get(lab, lab), bw[:, j]))

    if "gamma_party" in post:
        gp = flat(post["gamma_party"])  # (S, P)
        labels_p = {"party_populism_index_z": "Populism (+1 SD)",
                    "party_econ_left_right_z": "Econ right (+1 SD)",
                    "populism_x_rightleft_z": "Right-wing populism (+1 SD)"}
        for j, lab in enumerate(d["party_term_names"]):
            rows.append((labels_p.get(lab, lab), gp[:, j]))

    if "gamma_country_int" in post:
        gc = flat(post["gamma_country_int"])  # (S, C)
        labels_c = {"liberal_democracy_index_z": "Lib. democracy (+1 SD)",
                    "sm_hate_speech_z": "SM hate speech (+1 SD)"}
        for j, lab in enumerate(d["country_term_names"]):
            rows.append((labels_c.get(lab, lab), gc[:, j]))

    for param, label in [
        ("gamma_pop_opp",      "Populism × opposition"),
        ("gamma_pop_libdem",   "Populism × lib. democracy"),
        ("gamma_pop_time",     "Populism × time"),
        ("gamma_libdem_trend", "Libdem within-country change"),
    ]:
        if param in post:
            rows.append((label, flat(post[param])))

    names = [r[0] for r in rows]
    draws_list = [r[1] for r in rows]
    means = np.array([x.mean() for x in draws_list])
    lo    = np.array([np.quantile(x, 0.025) for x in draws_list])
    hi    = np.array([np.quantile(x, 0.975) for x in draws_list])
    y     = np.arange(len(names))[::-1]

    fig, ax = plt.subplots(figsize=(DOUBLE_W, max(3.0, 0.28 * len(names))))
    ax.hlines(y, lo, hi, linewidth=1.1, color="black")
    ax.plot(means, y, "o", color="black", markersize=5)
    ax.axvline(0.0, linewidth=0.8, linestyle="--", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels(names, fontsize=8)
    ax.set_xlabel("Effect on log-odds (95% HDI)")
    ax.set_title("Posterior main effects")

    # OR annotations
    for yi, m, lo_v, hi_v in zip(y, means, lo, hi):
        ax.annotate(
            f"OR={np.exp(m):.2f} [{np.exp(lo_v):.2f}, {np.exp(hi_v):.2f}]",
            xy=(hi_v, yi), xytext=(hi_v + 0.02, yi),
            fontsize=7, va="center",
        )

    fig.tight_layout()
    save_fig(fig, outfile)


def fig_time_trend_descriptive(pt: pd.DataFrame, outfile: Path) -> None:
    """
    Fig 2: Descriptive time trend — monthly share of very uncivil tweets
    across all countries and parties. Includes LOWESS smoother.
    """
    from statsmodels.nonparametric.smoothers_lowess import lowess

    ct = pt.copy()
    ct["month"] = pd.to_datetime(ct["month"])
    ct = ct.sort_values("month")
    monthly = ct.groupby("month").agg(
        total_tweets=("n_tweets", "sum"),
        total_y=("y_strong", "sum"),
    )
    monthly["rate"] = monthly["total_y"] / monthly["total_tweets"]

    months_num = (monthly.index - monthly.index[0]).days.astype(float)
    smoothed = lowess(monthly["rate"].values, months_num, frac=0.25, return_sorted=True)

    fig, ax = plt.subplots(figsize=(DOUBLE_W, DOUBLE_W * 0.5))
    ax.scatter(monthly.index, monthly["rate"] * 100,
               s=monthly["total_tweets"] / monthly["total_tweets"].max() * 30,
               color="black", alpha=0.6, zorder=3, label="Monthly average")
    smooth_dates = monthly.index[0] + pd.to_timedelta(smoothed[:, 0], unit="D")
    ax.plot(smooth_dates, smoothed[:, 1] * 100, color="black", linewidth=1.5,
            label="LOWESS smoother")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.set_xlabel("Month")
    ax.set_ylabel("Share of very uncivil tweets (%)")
    ax.set_title("Trend in elite political incivility (2017–2022)")
    ax.legend(fontsize=8)
    fig.tight_layout()
    save_fig(fig, outfile)


def fig_country_time_slopes(idata: az.InferenceData, d: dict, outfile: Path) -> None:
    """
    Fig 3: Country-specific time slopes (beta_t + b_country) with 95% HDI.
    Sorted by median slope.
    """
    post = stacked_posterior(idata)

    bt = flat(post["beta_t"])   # (S,)
    bc = flat(post["b_country"])  # (S, n_country)
    slopes = bt[:, None] + bc

    means = slopes.mean(axis=0)
    lo    = np.quantile(slopes, 0.025, axis=0)
    hi    = np.quantile(slopes, 0.975, axis=0)

    order = np.argsort(means)
    countries_sorted = [d["country_levels"][i] for i in order]

    y = np.arange(len(order))
    fig, ax = plt.subplots(figsize=(SINGLE_W, max(4.0, 0.22 * len(order))))
    ax.hlines(y, lo[order], hi[order], linewidth=0.9, color="black")
    ax.plot(means[order], y, "o", color="black", markersize=4)
    ax.axvline(0.0, linewidth=0.8, linestyle="--", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels(countries_sorted, fontsize=7)
    ax.set_xlabel("Country time slope (log-odds per SD time)")
    ax.set_title("Country-specific trends in incivility")
    fig.tight_layout()
    save_fig(fig, outfile)


def fig_country_time_series(pt: pd.DataFrame, outfile: Path,
                             max_countries: int = 26) -> None:
    """
    Fig 4: Small-multiple monthly incivility rate by country with LOWESS.
    """
    from statsmodels.nonparametric.smoothers_lowess import lowess
    import math

    pt2 = pt.copy()
    pt2["month"] = pd.to_datetime(pt2["month"])
    pt2 = pt2.sort_values(["country", "month"])

    countries = sorted(pt2["country"].unique())[:max_countries]
    ncols = 5
    nrows = math.ceil(len(countries) / ncols)

    fig, axes = plt.subplots(nrows, ncols, figsize=(DOUBLE_W * 1.5, nrows * 1.6),
                              sharey=False, sharex=True)
    axes_flat = axes.flatten()

    for ax, country in zip(axes_flat, countries):
        sub = (
            pt2[pt2["country"] == country]
            .groupby("month")
            .agg(n=("n_tweets", "sum"), y=("y_strong", "sum"))
            .assign(rate=lambda d: d["y"] / d["n"])
        )
        if len(sub) < 4:
            ax.set_visible(False)
            continue
        months_num = (sub.index - sub.index[0]).days.astype(float)
        sm = lowess(sub["rate"].values, months_num, frac=0.35, return_sorted=True)
        sm_dates = sub.index[0] + pd.to_timedelta(sm[:, 0], unit="D")
        ax.plot(sub.index, sub["rate"] * 100, color="gray", linewidth=0.6, alpha=0.7)
        ax.plot(sm_dates, sm[:, 1] * 100, color="black", linewidth=1.0)
        ax.set_title(country, fontsize=7, pad=2)
        ax.tick_params(labelsize=6)

    for ax in axes_flat[len(countries):]:
        ax.set_visible(False)

    fig.supxlabel("Month", fontsize=8, y=-0.01)
    fig.supylabel("Very uncivil tweets (%)", fontsize=8, x=0.01)
    fig.suptitle("Country-level trends in elite incivility (2017–2022)", fontsize=9)
    fig.tight_layout()
    save_fig(fig, outfile)


def fig_partyfamily_effects(idata_pf: az.InferenceData, outfile: Path,
                             pf_levels: list[str] | None = None) -> None:
    """
    Fig 5: Party-family effects on incivility, odds-ratio scale relative to
    the grand mean, sorted by median and annotated with OR [95% CI].
    """
    post = stacked_posterior(idata_pf)
    bpf  = flat(post["beta_partyfamily"])  # (S, K)
    if pf_levels is None:
        pf_levels = [f"fam_{i}" for i in range(bpf.shape[1])]
    or_samples = np.exp(bpf)

    medians = np.median(or_samples, axis=0)
    lo      = np.quantile(or_samples, 0.025, axis=0)
    hi      = np.quantile(or_samples, 0.975, axis=0)

    order = np.argsort(medians)
    y = np.arange(len(order))
    labels = [PARFAM_LABELS.get(pf_levels[i], pf_levels[i]) for i in order]

    fig, ax = plt.subplots(figsize=(DOUBLE_W, max(3.0, 0.35 * len(order))))
    ax.hlines(y, lo[order], hi[order], linewidth=1.2, color="black")
    ax.plot(medians[order], y, "o", color="black", markersize=5)
    ax.axvline(1.0, linewidth=0.8, linestyle="--", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    for j, i in enumerate(order):
        ax.text(hi[i] + 0.05, j, f"{medians[i]:.2f} [{lo[i]:.2f}–{hi[i]:.2f}]",
                fontsize=7, va="center")
    ax.set_xlabel("Party-family effect, odds ratio (95% CI) relative to grand mean")
    ax.set_title("Party-family effects on incivility")
    fig.tight_layout()
    save_fig(fig, outfile)


def fig_ppc(idata: az.InferenceData, d: dict, outfile: Path,
            n_samples: int = 200) -> None:
    """
    Fig 6: Posterior predictive check — observed vs predicted incivility rates.
    """
    if "posterior_predictive" not in idata.groups():
        print("  [skip] No posterior predictive in idata. Run with return ppc.")
        return

    y_obs  = d["y"].astype(float) / d["n"]
    pp = idata.posterior_predictive["y_obs"]
    if "chain" in pp.dims and "draw" in pp.dims:
        y_pred = pp.stack(sample=("chain", "draw")).values.T
    else:
        y_pred = pp.transpose("sample", ...).values
    if y_pred.shape[1] != len(d["n"]):
        print(f"  [skip] PPC shape mismatch ({y_pred.shape[1]} vs {len(d['n'])}); skipping.")
        return
    y_pred_rate = y_pred / d["n"]

    rng = np.random.default_rng(0)
    idx = rng.choice(y_pred_rate.shape[0], size=min(n_samples, y_pred_rate.shape[0]), replace=False)

    fig, ax = plt.subplots(figsize=(DOUBLE_W, DOUBLE_W * 0.5))
    for i in idx:
        ax.hist(y_pred_rate[i], bins=50, density=True, alpha=0.03,
                color="gray", histtype="stepfilled")
    ax.hist(y_obs, bins=50, density=True, alpha=0.8, color="black",
            histtype="step", linewidth=1.2, label="Observed")
    ax.set_xlabel("Incivility rate per cell")
    ax.set_ylabel("Density")
    ax.set_title("Posterior predictive check")
    ax.legend(fontsize=8)
    fig.tight_layout()
    save_fig(fig, outfile)


# Diagnostics
def print_diagnostics(idata: az.InferenceData) -> None:
    key_vars = [v for v in ["alpha", "beta_t", "beta_within", "gamma_party",
                             "gamma_country_int", "kappa", "sigma_party", "sigma_time"]
                if v in idata.posterior]
    summ = az.summary(idata, var_names=key_vars, hdi_prob=0.95)
    print("\nPosterior summary:")
    print(summ[["mean", "sd", "hdi_2.5%", "hdi_97.5%", "r_hat", "ess_bulk"]].round(3))
    divs = int(idata.sample_stats["diverging"].sum().values)
    print(f"\nDivergences: {divs}  (target: 0)")


# Topic model
TOPIC_LABELS = {
    "Crime":        "Crime & Justice",
    "Democracy":    "Democracy & Politics",
    "Economic":     "Economic Policy",
    "Education":    "Education",
    "Environment":  "Environment & Climate",
    "Identity":     "Identity & Culture",
    "Migration":    "Migration",
    "Nationalism":  "Nationalism",
    "Other":        "Other",
    "Security":     "Security & Defence",
    "Technology":   "Technology & Media",
    "Welfare":      "Welfare & Health",
}


def prepare_topic_data(tp: pd.DataFrame, min_tweets: int = 5, dv: str = "y_strong") -> dict:
    """
    Prepare model data from the party × country × month × topic panel.
    Uses pre-computed z-scores already in the panel.
    """
    df = tp.copy()
    df[dv] = pd.to_numeric(df[dv], errors="coerce")
    df["n_tweets"] = pd.to_numeric(df["n_tweets"], errors="coerce")
    df["month"] = pd.to_datetime(df["month"], errors="coerce")

    df = df.dropna(subset=[dv, "n_tweets", "country", "partyfactsid", "month", "topic"])
    df[dv] = df[dv].astype("int64")
    df["n_tweets"] = df["n_tweets"].astype("int64")
    df = df[
        (df["n_tweets"] >= min_tweets) &
        (df[dv] >= 0) &
        (df[dv] <= df["n_tweets"])
    ].reset_index(drop=True)

    # Group indices
    df["country_idx"] = pd.Categorical(df["country"]).codes.astype("int64")
    df["party_uid"]   = df["country"].astype(str) + "___" + df["partyfactsid"].astype(str)
    df["party_idx"]   = pd.Categorical(df["party_uid"]).codes.astype("int64")
    df["topic_idx"]   = pd.Categorical(df["topic"]).codes.astype("int64")

    country_levels   = pd.Categorical(df["country"]).categories
    party_uid_levels = pd.Categorical(df["party_uid"]).categories
    topic_levels     = list(pd.Categorical(df["topic"]).categories)

    n_country = int(df["country_idx"].max() + 1)
    n_party   = int(df["party_idx"].max() + 1)
    n_topic   = len(topic_levels)

    # Time
    month_levels = pd.Index(sorted(df["month"].unique()))
    df["t_idx"]   = df["month"].map({m: i for i, m in enumerate(month_levels)}).astype("int64")
    n_time = len(month_levels)
    t_raw  = df["t_idx"].astype("float64")
    df["t_slope"] = (t_raw - t_raw.mean()) / (t_raw.std(ddof=0) + 1e-12)

    # Within-obs vars
    for col in WITHIN_VARS:
        if col not in df.columns:
            df[col] = 0.0
        df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0.0).clip(0.0, 1.0).astype("float64")

    # Party-level Z (use pre-computed z-scores, fill missing with 0)
    party_term_names = ["party_populism_index_z", "party_econ_left_right_z", "populism_x_rightleft_z"]
    party_g = (
        df.groupby("party_uid")[["party_populism_z", "party_lr_z", "rwpop_interaction_z"]]
        .mean(numeric_only=True)
        .fillna(0.0)
    )
    party_Z = (
        party_g
        .rename(columns={"party_populism_z":   "party_populism_index_z",
                         "party_lr_z":          "party_econ_left_right_z",
                         "rwpop_interaction_z": "populism_x_rightleft_z"})
        .reindex(party_uid_levels)
        .fillna(0.0)
        .to_numpy(np.float64)
    )
    # pop_z per party for the interaction term
    pop_z_party = party_g["party_populism_z"].reindex(party_uid_levels).fillna(0.0).to_numpy(np.float64)

    # Country-level Z (libdem only)
    country_term_names = ["liberal_democracy_index_z"]
    country_g = df.groupby("country")[["libdem_z"]].mean(numeric_only=True).fillna(0.0)
    country_Z = (
        country_g
        .rename(columns={"libdem_z": "liberal_democracy_index_z"})
        .reindex(country_levels)
        .fillna(0.0)
        .to_numpy(np.float64)
    )

    print(f"  Topic panel: {len(df):,} obs, {n_country} countries, "
          f"{n_party} parties, {n_topic} topics, {n_time} months")

    return dict(
        df=df,
        y=df[dv].to_numpy(np.int64),
        n=df["n_tweets"].to_numpy(np.int64),
        Xw=df[WITHIN_VARS].to_numpy(np.float64),
        party_Z=party_Z,
        country_Z=country_Z,
        pop_z_party=pop_z_party,
        country_idx=df["country_idx"].to_numpy(np.int64),
        party_idx=df["party_idx"].to_numpy(np.int64),
        topic_idx=df["topic_idx"].to_numpy(np.int64),
        t_idx=df["t_idx"].to_numpy(np.int64),
        t_slope=df["t_slope"].to_numpy(np.float64),
        n_country=n_country,
        n_party=n_party,
        n_topic=n_topic,
        n_time=n_time,
        country_levels=list(country_levels),
        party_uid_levels=list(party_uid_levels),
        topic_levels=topic_levels,
        month_levels=month_levels,
        party_term_names=party_term_names,
        country_term_names=country_term_names,
        within_vars=WITHIN_VARS,
        dv=dv,
    )


def build_topic_model(d: dict) -> tuple[pm.Model, dict]:
    """
    Beta-Binomial topic model:
      - Topic random intercepts (partial pooling) — which topics are most incivil?
      - Topic × populism varying slopes — do populists concentrate incivility on specific topics?
      - Country RE (intercept only, explained by libdem; no LKJ for tractability)
      - Party RE (explained by populism + L/R + interaction)
      - Within-obs vars, month shocks, global time trend
    """
    n_obs = len(d["y"])
    coords = {
        "obs":          np.arange(n_obs),
        "time":         np.arange(d["n_time"]),
        "country":      np.arange(d["n_country"]),
        "party":        np.arange(d["n_party"]),
        "topic":        d["topic_levels"],
        "within_term":  d["within_vars"],
        "party_term":   d["party_term_names"],
        "country_term": d["country_term_names"],
    }

    with pm.Model(coords=coords) as model:
        # Data
        y_d    = pm.Data("y",           d["y"],            dims="obs")
        n_d    = pm.Data("n",           d["n"],            dims="obs")
        Xw_d   = pm.Data("Xw",          d["Xw"],           dims=("obs", "within_term"))
        pZ_d   = pm.Data("party_Z",     d["party_Z"],      dims=("party", "party_term"))
        cZ_d   = pm.Data("country_Z",   d["country_Z"],    dims=("country", "country_term"))
        popz_d = pm.Data("pop_z_party", d["pop_z_party"],  dims="party")
        ci_d   = pm.Data("country_idx", d["country_idx"],  dims="obs")
        pi_d   = pm.Data("party_idx",   d["party_idx"],    dims="obs")
        ti_d   = pm.Data("t_idx",       d["t_idx"],        dims="obs")
        ts_d   = pm.Data("t_slope",     d["t_slope"],      dims="obs")
        topi_d = pm.Data("topic_idx",   d["topic_idx"],    dims="obs")

        # Population-level
        alpha  = pm.Normal("alpha",   0.0, 1.2)
        beta_t = pm.Normal("beta_t",  0.0, 0.20)
        beta_w = pm.Normal("beta_within", 0.0, 0.25, dims="within_term")

        # Month shocks
        sigma_time = pm.Exponential("sigma_time", 5.0)
        a_time_raw = pm.Normal("a_time_raw", 0.0, sigma_time, dims="time")
        a_time     = pm.Deterministic(
            "a_time", a_time_raw - pm.math.mean(a_time_raw), dims="time"
        )

        # Country RE (intercept only)
        gamma_c   = pm.Normal("gamma_country", 0.0, 0.25, dims="country_term")
        sigma_c   = pm.Exponential("sigma_country", 2.0)
        z_c       = pm.Normal("z_country", 0.0, 1.0, dims="country")
        a_country = pm.Deterministic(
            "a_country",
            pm.math.dot(cZ_d, gamma_c) + z_c * sigma_c,
            dims="country",
        )

        # Party RE (explained by populism + L/R + interaction)
        gamma_p = pm.Normal("gamma_party", 0.0, 0.25, dims="party_term")
        sigma_p = pm.Exponential("sigma_party", 2.0)
        z_p     = pm.Normal("z_party", 0.0, 1.0, dims="party")
        a_party = pm.Deterministic(
            "a_party",
            pm.math.dot(pZ_d, gamma_p) + z_p * sigma_p,
            dims="party",
        )

        # Topic random intercepts
        sigma_topic = pm.Exponential("sigma_topic", 2.0)
        z_topic     = pm.Normal("z_topic", 0.0, 1.0, dims="topic")
        a_topic     = pm.Deterministic("a_topic", z_topic * sigma_topic, dims="topic")

        # Topic × populism varying slopes
        # Global populism effect (shared across topics)
        mu_pop    = pm.Normal("mu_pop_topic", 0.0, 0.25)
        sigma_pop = pm.Exponential("sigma_pop_topic", 2.0)
        z_pop     = pm.Normal("z_pop_topic", 0.0, 1.0, dims="topic")
        b_pop_topic = pm.Deterministic(
            "b_pop_topic",
            mu_pop + z_pop * sigma_pop,
            dims="topic",
        )

        # Linear predictor
        eta = (
            alpha
            + pm.math.dot(Xw_d, beta_w)
            + beta_t * ts_d
            + a_time[ti_d]
            + a_country[ci_d]
            + a_party[pi_d]
            + a_topic[topi_d]
            + b_pop_topic[topi_d] * popz_d[pi_d]
        )

        p      = pm.Deterministic("p", pm.math.sigmoid(eta), dims="obs")
        p_clip = pm.math.clip(p, 1e-6, 1.0 - 1e-6)
        kappa  = pm.Exponential("kappa", 1.0 / 50)
        pm.BetaBinomial(
            "y_obs",
            n=n_d,
            alpha=p_clip * kappa,
            beta=(1.0 - p_clip) * kappa,
            observed=y_d,
            dims="obs",
        )

    return model, coords


def fit_topic_model(d: dict, draws: int = 2000, tune: int = 2000,
                    chains: int = 4, target_accept: float = 0.99) -> az.InferenceData:
    model, _ = build_topic_model(d)
    with model:
        idata = pm.sample(
            draws=draws, tune=tune, chains=chains,
            target_accept=target_accept, max_treedepth=12,
            random_seed=42, nuts_sampler="numpyro",
            progressbar=True, return_inferencedata=True,
        )
    return idata


def fig_topic_intercepts(idata: az.InferenceData, d: dict, outfile: Path) -> None:
    """Topic random intercepts — which topics are intrinsically more incivil."""
    post = stacked_posterior(idata)
    at   = flat(post["a_topic"])   # (S, n_topic)

    means = at.mean(axis=0)
    lo    = np.quantile(at, 0.025, axis=0)
    hi    = np.quantile(at, 0.975, axis=0)
    order = np.argsort(means)

    labels = [TOPIC_LABELS.get(d["topic_levels"][i], d["topic_levels"][i]) for i in order]
    y = np.arange(len(order))

    fig, ax = plt.subplots(figsize=(DOUBLE_W, max(3.0, 0.32 * len(order))))
    ax.hlines(y, lo[order], hi[order], linewidth=1.1, color="black")
    ax.plot(means[order], y, "o", color="black", markersize=5)
    ax.axvline(0.0, linewidth=0.8, linestyle="--", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    ax.set_xlabel("Topic random intercept — log-odds vs. mean (95% HDI)")
    ax.set_title("Topic-level incivility (controlling for party, country, time)")
    fig.tight_layout()
    save_fig(fig, outfile)


def fig_topic_populism_slopes(idata: az.InferenceData, d: dict, outfile: Path) -> None:
    """Topic × populism varying slopes — where do populists concentrate incivility?"""
    post = stacked_posterior(idata)
    bp   = flat(post["b_pop_topic"])   # (S, n_topic)
    mu   = flat(post["mu_pop_topic"])  # (S,)

    means = bp.mean(axis=0)
    lo    = np.quantile(bp, 0.025, axis=0)
    hi    = np.quantile(bp, 0.975, axis=0)
    order = np.argsort(means)

    labels = [TOPIC_LABELS.get(d["topic_levels"][i], d["topic_levels"][i]) for i in order]
    y = np.arange(len(order))

    fig, ax = plt.subplots(figsize=(DOUBLE_W, max(3.0, 0.32 * len(order))))
    ax.hlines(y, lo[order], hi[order], linewidth=1.1, color="black")
    ax.plot(means[order], y, "o", color="black", markersize=5)
    # Global mean as reference
    ax.axvline(float(mu.mean()), linewidth=1.0, linestyle="--", color="#555555",
               label=f"Global mean (OR={np.exp(mu.mean()):.2f})")
    ax.axvline(0.0, linewidth=0.6, linestyle=":", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    ax.set_xlabel("Populism × topic slope — log-odds per SD populism (95% HDI)")
    ax.set_title("Where do populist parties concentrate incivility?")
    ax.legend(fontsize=7)
    fig.tight_layout()
    save_fig(fig, outfile)


def compute_topic_effects_table(idata: az.InferenceData, d: dict) -> pd.DataFrame:
    """Topic intercepts + populism slopes as a tidy table."""
    post  = stacked_posterior(idata)
    at    = flat(post["a_topic"])    # (S, n_topic)
    bp    = flat(post["b_pop_topic"])  # (S, n_topic)

    rows = []
    for j, topic in enumerate(d["topic_levels"]):
        lo_a, hi_a = hdi95(at[:, j])
        lo_b, hi_b = hdi95(bp[:, j])
        rows.append({
            "topic":           topic,
            "topic_label":     TOPIC_LABELS.get(topic, topic),
            "intercept_mean":  float(at[:, j].mean()),
            "intercept_lo":    lo_a,
            "intercept_hi":    hi_a,
            "intercept_or":    float(np.exp(at[:, j].mean())),
            "pop_slope_mean":  float(bp[:, j].mean()),
            "pop_slope_lo":    lo_b,
            "pop_slope_hi":    hi_b,
            "pop_slope_or":    float(np.exp(bp[:, j].mean())),
        })
    df = pd.DataFrame(rows).sort_values("intercept_mean", ascending=False)
    print("\nTopic intercepts & populism slopes:")
    print(df[["topic_label", "intercept_or", "pop_slope_or"]].round(3).to_string(index=False))
    return df


# CLI entry point
def parse_args():
    p = argparse.ArgumentParser(description="Elite incivility Bayesian analysis")
    p.add_argument("--data", default="party_country_month_panel_v4.df.pkl",
                   help="Path to party × country × month panel (.pkl or .parquet)")
    p.add_argument("--outdir", default="results", help="Output directory")
    p.add_argument("--dv", choices=["y_strong", "y_any"], default="y_strong",
                   help="Dependent variable (default: y_strong)")
    p.add_argument("--min-tweets", type=int, default=5, help="Min tweets per cell")
    p.add_argument("--draws", type=int, default=2000)
    p.add_argument("--tune",  type=int, default=2000)
    p.add_argument("--chains", type=int, default=4)
    p.add_argument("--target-accept", type=float, default=0.99, help="NUTS target acceptance rate")
    p.add_argument("--load-main",  help="Load saved idata (skip fitting main model)")
    p.add_argument("--load-pf",    help="Load saved idata (skip fitting party-family model)")
    p.add_argument("--skip-pf",    action="store_true", help="Skip party-family model")
    p.add_argument("--topic-data", default=None,
                   help="Path to topic incivility panel (.pkl or .parquet)")
    p.add_argument("--load-topic", help="Load saved topic idata (skip fitting)")
    p.add_argument("--skip-topic", action="store_true", help="Skip topic model")
    p.add_argument("--load-pop-opp",    help="Load saved pop×opposition idata (skip fitting)")
    p.add_argument("--skip-pop-opp",    action="store_true", help="Skip populism × opposition model")
    p.add_argument("--load-pop-libdem", help="Load saved pop×libdem idata (skip fitting)")
    p.add_argument("--skip-pop-libdem", action="store_true", help="Skip populism × lib-democracy model")
    p.add_argument("--load-pop-time",       help="Load saved pop×time idata (skip fitting)")
    p.add_argument("--skip-pop-time",       action="store_true", help="Skip populism × time model")
    p.add_argument("--load-libdem-trend",   help="Load saved libdem-trend idata (skip fitting)")
    p.add_argument("--skip-libdem-trend",   action="store_true", help="Skip democratic backsliding model")
    p.add_argument("--persp-data",          default=None,
                   help="Path to Perspective toxicity panel (.parquet)")
    p.add_argument("--load-persp",          help="Load saved Perspective idata (skip fitting)")
    p.add_argument("--skip-persp",          action="store_true", help="Skip Perspective robustness model")
    p.add_argument("--figs-only",  action="store_true", help="Only regenerate figures")
    return p.parse_args()


def main():
    args = parse_args()
    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    set_pub_style(base_font=9)

    # Load data
    data_path = Path(args.data)
    print(f"Loading data from {data_path} …")
    if data_path.suffix == ".pkl":
        pt = pd.read_pickle(data_path)
    else:
        pt = pd.read_parquet(data_path)

    print(f"  {len(pt):,} obs, {pt['country'].nunique()} countries, "
          f"{pt['partyfactsid'].nunique()} parties")

    d = prepare_data(pt, min_tweets=args.min_tweets, dv=args.dv)
    print(f"  After filtering (min_tweets={args.min_tweets}): {len(d['y']):,} obs")

    # Descriptive figure (no model needed)
    fig_time_trend_descriptive(pt, outdir / "fig_time_trend_descriptive.pdf")
    fig_country_time_series(pt, outdir / "fig_country_time_series.pdf")

    if args.figs_only and args.load_main is None:
        print("--figs-only but --load-main not set; exiting after descriptive figs.")
        return

    # Main model
    if args.load_main:
        print(f"Loading main model idata from {args.load_main} …")
        idata = az.from_netcdf(args.load_main)
    else:
        print("Fitting main model …")
        idata = fit_main_model(d, draws=args.draws, tune=args.tune, chains=args.chains,
                               target_accept=args.target_accept, outdir=outdir)
        idata.to_netcdf(str(outdir / "idata_main.nc"))
        print(f"  saved → {outdir / 'idata_main.nc'}")

    print_diagnostics(idata)
    eff = compute_effects_table(idata, d)
    eff.to_csv(outdir / "effects_summary.csv", index=False)

    fig_forest_effects(idata, d, outdir / "fig_forest_effects.pdf")
    fig_country_time_slopes(idata, d, outdir / "fig_country_time_slopes.pdf")
    fig_ppc(idata, d, outdir / "fig_ppc.pdf")

    # Party-family model
    if not args.skip_pf:
        if args.load_pf:
            print(f"Loading PF model idata from {args.load_pf} …")
            idata_pf = az.from_netcdf(args.load_pf)
        else:
            print("Fitting party-family model …")
            idata_pf = fit_partyfamily_model(d, draws=args.draws, tune=args.tune, chains=args.chains,
                                             target_accept=args.target_accept)
            idata_pf.to_netcdf(str(outdir / "idata_partyfamily.nc"))
        fig_partyfamily_effects(idata_pf, outdir / "fig_partyfamily_effects.pdf",
                                pf_levels=d["pf_levels"])

    # Interaction robustness models
    for label, skip_flag, load_attr, build_fn, fit_fn, nc_name, csv_name, pdf_name in [
        ("pop × opposition",      args.skip_pop_opp,    args.load_pop_opp,
         build_pop_opp_model,    fit_pop_opp_model,
         "idata_pop_opp.nc",     "effects_pop_opp.csv",    "fig_forest_pop_opp.pdf"),
        ("pop × lib-democracy",   args.skip_pop_libdem, args.load_pop_libdem,
         build_pop_libdem_model, fit_pop_libdem_model,
         "idata_pop_libdem.nc",  "effects_pop_libdem.csv", "fig_forest_pop_libdem.pdf"),
        ("pop × time",            args.skip_pop_time,   args.load_pop_time,
         build_pop_time_model,   fit_pop_time_model,
         "idata_pop_time.nc",    "effects_pop_time.csv",   "fig_forest_pop_time.pdf"),
        ("democratic backsliding", args.skip_libdem_trend, args.load_libdem_trend,
         build_libdem_trend_model, fit_libdem_trend_model,
         "idata_libdem_trend.nc", "effects_libdem_trend.csv", "fig_forest_libdem_trend.pdf"),
    ]:
        if skip_flag:
            print(f"\n[skip] {label} model (--skip flag set).")
            continue
        if load_attr:
            print(f"\nLoading {label} model from {load_attr} …")
            idata_int = az.from_netcdf(load_attr)
        else:
            print(f"\nFitting {label} model …")
            idata_int = fit_fn(d, draws=args.draws, tune=args.tune,
                               chains=args.chains, target_accept=args.target_accept)
            idata_int.to_netcdf(str(outdir / nc_name))
            print(f"  saved → {outdir / nc_name}")
        print_diagnostics(idata_int)
        eff_int = compute_effects_table(idata_int, d)
        eff_int.to_csv(outdir / csv_name, index=False)
        fig_forest_effects(idata_int, d, outdir / pdf_name)

    # Perspective robustness model
    if not args.skip_persp:
        persp_path = Path(args.persp_data) if args.persp_data else None
        if persp_path is None:
            print("\n[skip] --persp-data not provided; skipping Perspective model.")
        else:
            print(f"\nLoading Perspective panel from {persp_path} …")
            pp = pd.read_parquet(persp_path)
            dp = prepare_perspective_data(pp, min_tweets=args.min_tweets)
            if args.load_persp:
                print(f"Loading Perspective idata from {args.load_persp} …")
                idata_persp = az.from_netcdf(args.load_persp)
            else:
                print("Fitting Perspective model …")
                idata_persp = fit_perspective_model(
                    dp, draws=args.draws, tune=args.tune,
                    chains=args.chains, target_accept=args.target_accept,
                )
                idata_persp.to_netcdf(str(outdir / "idata_persp.nc"))
                print(f"  saved → {outdir / 'idata_persp.nc'}")
            print_diagnostics(idata_persp)
            eff_persp = compute_effects_table(idata_persp, dp)
            eff_persp.to_csv(outdir / "effects_persp.csv", index=False)
            fig_forest_effects(idata_persp, dp, outdir / "fig_forest_persp.pdf")

    # Topic model
    if not args.skip_topic:
        topic_path = Path(args.topic_data) if args.topic_data else None
        if topic_path is None:
            print("\n[skip] --topic-data not provided; skipping topic model.")
        else:
            print(f"\nLoading topic panel from {topic_path} …")
            if topic_path.suffix == ".pkl":
                tp = pd.read_pickle(topic_path)
            else:
                tp = pd.read_parquet(topic_path)

            dt = prepare_topic_data(tp, min_tweets=args.min_tweets, dv=args.dv)

            if args.load_topic:
                print(f"Loading topic model idata from {args.load_topic} …")
                idata_topic = az.from_netcdf(args.load_topic)
            else:
                print("Fitting topic model …")
                idata_topic = fit_topic_model(
                    dt, draws=args.draws, tune=args.tune,
                    chains=args.chains, target_accept=args.target_accept,
                )
                idata_topic.to_netcdf(str(outdir / "idata_topic.nc"))
                print(f"  saved → {outdir / 'idata_topic.nc'}")

            divs = int(idata_topic.sample_stats["diverging"].sum().values)
            print(f"  Divergences: {divs}")

            teff = compute_topic_effects_table(idata_topic, dt)
            teff.to_csv(outdir / "topic_effects_summary.csv", index=False)

            fig_topic_intercepts(idata_topic, dt, outdir / "fig_topic_intercepts.pdf")
            fig_topic_populism_slopes(idata_topic, dt, outdir / "fig_topic_populism_slopes.pdf")

    print("\nDone. Outputs in:", outdir)


if __name__ == "__main__":
    main()
