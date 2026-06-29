"""
Simplified, bug-fixed party-family model.

The original build_partyfamily_model() in analysis_incivility.py has two
problems that together produced catastrophic non-convergence (r_hat 3-4.5,
ess_bulk ~4 across the board):

  1. A genuine indexing bug: the per-observation family lookup used
     beta_pf[pi_d] (pi_d = party_idx, 243 levels) instead of
     beta_pf[pf_d] (pf_d = pf_idx, 11 levels). beta_pf only has 11
     elements, so this silently read out-of-bounds/garbage family effects
     for most parties (JAX clips rather than raising on out-of-bounds
     indexing).
  2. A genuinely hard-to-identify structure even once fixed: a separate
     per-party residual (z_party * sigma_party) competes with the family
     fixed effect for families with as few as 6-8 member parties, and a
     correlated LKJ country intercept+slope adds further difficult
     geometry that isn't needed just to compare family-level effects.

This script fixes (1) and drops the problematic pieces of (2), leaving a
leaner model whose only job is a clean, well-identified family-level
comparison: family fixed effects (hierarchically shrunk) plus simple
independent country and month intercepts, controlling for the same
within-observation covariates as the main model.

Usage
-----
    python refit_partyfamily_simple.py \\
        --data party_country_month_panel_v4.df.pkl \\
        --outdir results_v4

Output
------
    idata_partyfamily_simple.nc          posterior samples
    fig_partyfamily_effects_simple.pdf   family-level OR forest plot
"""

from __future__ import annotations

import argparse
from pathlib import Path

import arviz as az
import numpy as np
import pandas as pd
import pymc as pm
import matplotlib.pyplot as plt

from analysis_incivility import prepare_data, set_pub_style, save_fig


def build_partyfamily_simple_model(d: dict) -> tuple[pm.Model, dict]:
    n_obs = len(d["y"])
    coords = {
        "obs": np.arange(n_obs),
        "time": np.arange(d["n_time"]),
        "country": np.arange(d["n_country"]),
        "partyfamily": d["pf_levels"],
        "within_term": d["within_vars"],
    }

    with pm.Model(coords=coords) as model:
        y_d  = pm.Data("y",          d["y"],           dims="obs")
        n_d  = pm.Data("n",          d["n"],           dims="obs")
        Xw_d = pm.Data("Xw",         d["Xw"],          dims=("obs", "within_term"))
        ci_d = pm.Data("country_idx", d["country_idx"], dims="obs")
        ti_d = pm.Data("t_idx",      d["t_idx"],        dims="obs")
        ts_d = pm.Data("t_slope",    d["t_slope"],      dims="obs")
        pf_d = pm.Data("pf_idx",     d["pf_idx"],       dims="obs")

        alpha  = pm.Normal("alpha",    0.0, 1.2)
        beta_t = pm.Normal("beta_t",   0.0, 0.20)
        beta_w = pm.Normal("beta_within", 0.0, 0.25, dims="within_term")

        sigma_time = pm.Exponential("sigma_time", 5.0)
        a_time_raw = pm.Normal("a_time_raw", 0.0, sigma_time, dims="time")
        a_time = pm.Deterministic(
            "a_time", a_time_raw - pm.math.mean(a_time_raw), dims="time"
        )

        sigma_country = pm.Exponential("sigma_country", 5.0)
        a_country_raw = pm.Normal("a_country_raw", 0.0, sigma_country, dims="country")
        a_country = pm.Deterministic(
            "a_country", a_country_raw - pm.math.mean(a_country_raw), dims="country"
        )

        # Family fixed effects, hierarchically shrunk toward the grand mean.
        sigma_pf = pm.HalfNormal("sigma_partyfamily", 0.5)
        beta_pf  = pm.Normal("beta_partyfamily", 0.0, sigma_pf, dims="partyfamily")

        eta = (
            alpha
            + pm.math.dot(Xw_d, beta_w)
            + a_time[ti_d]
            + a_country[ci_d]
            + beta_pf[pf_d]   # fixed: index by family (pf_d), not party (pi_d)
            + beta_t * ts_d
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


def fit_partyfamily_simple_model(d: dict, draws: int = 2000, tune: int = 2000,
                                  chains: int = 4, target_accept: float = 0.95) -> az.InferenceData:
    model, _ = build_partyfamily_simple_model(d)
    with model:
        idata = pm.sample(
            draws=draws, tune=tune, chains=chains,
            target_accept=target_accept,
            nuts_sampler="numpyro",
            idata_kwargs={"log_likelihood": False},
            random_seed=0,
        )
    return idata


PARFAM_LABELS = {
    10: "Green/Ecologist", 20: "Radical left", 30: "Social democrat",
    40: "Liberal", 50: "Christian democrat", 60: "Conservative",
    70: "Nationalist/Radical right", 80: "Agrarian", 90: "Ethnic/Regional",
    95: "Single issue", 98: "Other",
}


def fig_partyfamily_effects_or(idata: az.InferenceData, outfile: Path, pf_levels) -> None:
    post = idata.posterior["beta_partyfamily"]
    bpf  = post.stack(sample=("chain", "draw")).values  # (K, S)
    or_samples = np.exp(bpf)

    medians = np.median(or_samples, axis=1)
    lo = np.quantile(or_samples, 0.025, axis=1)
    hi = np.quantile(or_samples, 0.975, axis=1)

    labels = [PARFAM_LABELS.get(int(l), str(l)) for l in pf_levels]
    order = np.argsort(medians)

    fig, ax = plt.subplots(figsize=(7.1, max(3.0, 0.35 * len(order))))
    y = np.arange(len(order))
    ax.hlines(y, lo[order], hi[order], linewidth=1.2, color="black")
    ax.plot(medians[order], y, "o", color="black", markersize=5)
    ax.axvline(1.0, linewidth=0.8, linestyle="--", color="gray")
    ax.set_yticks(y)
    ax.set_yticklabels([labels[i] for i in order], fontsize=8)
    for j, i in enumerate(order):
        ax.text(hi[i] + 0.05, j, f"{medians[i]:.2f} [{lo[i]:.2f}–{hi[i]:.2f}]",
                fontsize=7, va="center")
    ax.set_xlabel("Party-family effect, odds ratio (95% CI) relative to grand mean")
    ax.set_title("Party-family effects on incivility")
    fig.tight_layout()
    save_fig(fig, outfile)

    print("\nFamily-level OR [95% CI]:")
    for i in order:
        print(f"  {labels[i]:28s} {medians[i]:.3f} [{lo[i]:.3f}, {hi[i]:.3f}]")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data", default="party_country_month_panel_v4.df.pkl")
    p.add_argument("--outdir", default="results_v4")
    p.add_argument("--min-tweets", type=int, default=10)
    p.add_argument("--draws", type=int, default=2000)
    p.add_argument("--tune", type=int, default=2000)
    p.add_argument("--chains", type=int, default=4)
    p.add_argument("--target-accept", type=float, default=0.95)
    args = p.parse_args()

    set_pub_style(base_font=9)
    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    data_path = Path(args.data)
    pt = pd.read_pickle(data_path) if data_path.suffix == ".pkl" else pd.read_parquet(data_path)
    d = prepare_data(pt, min_tweets=args.min_tweets, dv="y_strong")
    print(f"Prepared {len(d['y']):,} obs, {len(d['pf_levels'])} families")

    idata = fit_partyfamily_simple_model(
        d, draws=args.draws, tune=args.tune, chains=args.chains,
        target_accept=args.target_accept,
    )
    idata.to_netcdf(str(outdir / "idata_partyfamily_simple.nc"))

    summ = az.summary(idata, var_names=["beta_partyfamily", "sigma_partyfamily"], hdi_prob=0.95)
    pd.set_option("display.width", 200)
    print("\nPosterior summary (family effects):")
    print(summ)
    print("\nDivergences:", int(idata.sample_stats.diverging.sum()))
    full = az.summary(idata, hdi_prob=0.95)
    print("Max r_hat overall:", full["r_hat"].max())
    print("Min ess_bulk overall:", full["ess_bulk"].min())

    fig_partyfamily_effects_or(idata, outdir / "fig_partyfamily_effects_simple.pdf", d["pf_levels"])
    print("\nDone.")


if __name__ == "__main__":
    main()
