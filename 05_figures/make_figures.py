"""
Builds the descriptive figures that sit alongside the model results in
results_v4/: the overall time-trend plot, party-family trend lines, and
the shift-share decomposition (behavioral vs. compositional change).
Also prints the summary numbers that get quoted in the manuscript text.
"""

import warnings
warnings.filterwarnings("ignore")

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from pathlib import Path
from statsmodels.nonparametric.smoothers_lowess import lowess

# paths (repo root = parent of this script's 05_figures/ directory)
BASE = Path(__file__).resolve().parents[1]
PANEL = BASE / "2026aggregated/party_country_month_panel_v4.df.pkl"
OUT   = BASE / "results_v4"
OUT.mkdir(exist_ok=True)

# load panel
df = pd.read_pickle(PANEL)
df["month_dt"] = pd.to_datetime(df["month"])

# family mapping (ParlGov parfam codes)
FAM_MAP = {
    10: "Green/Left",
    20: "Radical Left",
    30: "Social Democratic",
    40: "Liberal",
    50: "Christian Democratic",
    60: "Conservative",
    70: "Nationalist/Radical Right",
    80: "Special",
    90: "Agrarian",
    95: "Regional",
    98: "Special (other)",
}
# Simplified labels for plotting
FAM_LABEL = {
    10: "Green/Left",
    20: "Radical Left",
    30: "Social Dem.",
    40: "Liberal",
    50: "Christian Dem.",
    60: "Conservative",
    70: "Nationalist/Rad. Right",
}
MAIN_FAMS = list(FAM_LABEL.keys())

df["family"]      = df["party_mf_parfam"].map(FAM_MAP)
df["family_label"] = df["party_mf_parfam"].map(FAM_LABEL)

# global monthly rate (tweet-weighted)
monthly = (df.groupby("month_dt")
             .apply(lambda x: pd.Series({
                 "rate": x["y_strong"].sum() / x["n_tweets"].sum() * 100,
                 "n":    x["n_tweets"].sum(),
             }))
             .reset_index())

print("Global trend:")
print(f"First 3 months avg: {monthly.head(3)['rate'].mean():.2f}%")
print(f"Last 3 months avg:  {monthly.tail(3)['rate'].mean():.2f}%")

# LOWESS smoother
x = np.array(range(len(monthly)))
y = monthly["rate"].values
ys = lowess(y, x, frac=0.25, return_sorted=False)
print(f"LOWESS first pt: {ys[0]:.2f}%  last pt: {ys[-1]:.2f}%")
print(f"LOWESS min: {ys.min():.2f}%  max: {ys.max():.2f}%")
print(f"2017 LOWESS avg: {ys[:8].mean():.2f}%")
print(f"2021-22 LOWESS avg: {ys[-15:].mean():.2f}%")

# FIGURE 1: Global time trend
fig, ax = plt.subplots(figsize=(8, 4))

# scatter points scaled by n_tweets
sizes = (monthly["n"].values / monthly["n"].max()) * 120 + 5
ax.scatter(monthly["month_dt"], monthly["rate"], s=sizes, color="#4878d0",
           alpha=0.55, zorder=3, linewidth=0)
ax.plot(monthly["month_dt"], ys, color="#222222", lw=2.2, zorder=4,
        label="LOWESS smoother")

# COVID shading
import matplotlib.dates as mdates
covid_start = pd.Timestamp("2020-03-01")
covid_end   = pd.Timestamp("2020-07-01")
ax.axvspan(covid_start, covid_end, alpha=0.12, color="orange", label="COVID shock (Mar–Jun 2020)")

ax.set_xlabel("Month", fontsize=11)
ax.set_ylabel("Very uncivil tweets (% of total)", fontsize=11)
ax.set_title("Rising political incivility across 26 Western democracies, 2017–2022", fontsize=12)
ax.yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:.1f}%"))
ax.legend(fontsize=9, loc="upper left")
ax.grid(axis="y", alpha=0.3)
ax.set_xlim(monthly["month_dt"].min(), monthly["month_dt"].max())
fig.tight_layout()
fig.savefig(OUT / "fig_time_trend_descriptive.pdf", bbox_inches="tight")
plt.close()
print("saved fig_time_trend_descriptive.pdf")


# FIGURE 2: Family-level trends
# tweet-weighted rate per family × month
fam_monthly = (df[df["party_mf_parfam"].isin(MAIN_FAMS)]
               .groupby(["month_dt", "party_mf_parfam"])
               .apply(lambda x: pd.Series({
                   "rate": x["y_strong"].sum() / x["n_tweets"].sum() * 100,
                   "n":    x["n_tweets"].sum(),
               }))
               .reset_index())

print("\nFamily rates (overall):")
fam_overall = (df[df["party_mf_parfam"].isin(MAIN_FAMS)]
               .groupby("party_mf_parfam")
               .apply(lambda x: pd.Series({
                   "rate": x["y_strong"].sum() / x["n_tweets"].sum() * 100,
                   "first3": None,
                   "last15": None,
               }))
               .reset_index())
# First 3 months and last 3 months
for fam in MAIN_FAMS:
    sub = fam_monthly[fam_monthly["party_mf_parfam"] == fam].sort_values("month_dt")
    label = FAM_LABEL[fam]
    if len(sub) < 6:
        continue
    f3 = sub.head(3)["rate"].mean()
    l3 = sub.tail(3)["rate"].mean()
    print(f"  {label}: first3={f3:.2f}%, last3={l3:.2f}%, delta={l3-f3:+.2f}pp")

# Colour palette
COLORS = {
    70: "#d62728",   # red – nationalist/rad right
    20: "#9467bd",   # purple – radical left
    30: "#e377c2",   # pink – social dem
    60: "#8c564b",   # brown – conservative
    40: "#17becf",   # cyan – liberal
    50: "#bcbd22",   # yellow-green – christian dem
    10: "#2ca02c",   # green – green/left
}
LWIDTHS = {70: 2.8, 20: 1.8, 30: 1.8, 60: 1.8, 40: 1.3, 50: 1.3, 10: 1.3}

fig, ax = plt.subplots(figsize=(8.5, 5))

for fam in MAIN_FAMS:
    sub = fam_monthly[fam_monthly["party_mf_parfam"] == fam].sort_values("month_dt")
    if len(sub) < 12:
        continue
    xs = np.array(range(len(sub)))
    ys_f = lowess(sub["rate"].values, xs, frac=0.35, return_sorted=False)
    ax.plot(sub["month_dt"], ys_f,
            color=COLORS[fam], lw=LWIDTHS[fam],
            label=FAM_LABEL[fam],
            alpha=0.9, zorder=(4 if fam == 70 else 3))

ax.set_xlabel("Month", fontsize=11)
ax.set_ylabel("Very uncivil tweets (% of total)", fontsize=11)
ax.set_title("Rhetorical convergence: incivility trends by party family", fontsize=12)
ax.yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:.1f}%"))
ax.legend(fontsize=8, loc="upper left", ncol=2)
ax.grid(axis="y", alpha=0.3)
ax.set_xlim(monthly["month_dt"].min(), monthly["month_dt"].max())
fig.tight_layout()
fig.savefig(OUT / "fig_family_trends.pdf", bbox_inches="tight")
plt.close()
print("saved fig_family_trends.pdf")


# FIGURE 3: Shift-share decomposition
# Use only main 7 families for the decomposition
df_main = df[df["party_mf_parfam"].isin(MAIN_FAMS)].copy()

# Monthly family-level rates and volume shares
fam_vol = (df_main.groupby(["month_dt", "party_mf_parfam"])
           .agg(y=("y_strong", "sum"), n=("n_tweets", "sum"))
           .reset_index())

# baseline: 2017 average for each family
base_months = fam_vol[fam_vol["month_dt"].dt.year == 2017]
base_rate = (base_months.groupby("party_mf_parfam")
             .apply(lambda x: pd.Series({"base_r": x["y"].sum() / x["n"].sum() * 100}))
             .reset_index())
base_vol  = (base_months.groupby("party_mf_parfam")["n"].sum()
             .rename("base_n"))
base_total = base_months["n"].sum()
base_w = (base_vol / base_total).rename("base_w")

fam_vol = fam_vol.merge(base_rate, on="party_mf_parfam")
fam_vol = fam_vol.merge(base_w, on="party_mf_parfam")

# total tweets per month for share computation
month_total = fam_vol.groupby("month_dt")["n"].sum().rename("n_total")
fam_vol = fam_vol.merge(month_total, on="month_dt")
fam_vol["w_t"] = fam_vol["n"] / fam_vol["n_total"]
fam_vol["r_t"] = fam_vol["y"] / fam_vol["n"] * 100

# aggregate observed and counterfactuals per month
agg = (fam_vol.groupby("month_dt")
       .apply(lambda x: pd.Series({
           "R_obs":  (x["w_t"] * x["r_t"]).sum(),               # actual
           "R_behav": (x["base_w"] * x["r_t"]).sum(),           # behavioral only (fixed 2017 comp)
           "R_comp":  (x["w_t"] * x["base_r"]).sum(),           # compositional only (fixed 2017 rates)
       }))
       .reset_index())

# baseline: mean 2017 observed rate
R_2017 = agg[agg["month_dt"].dt.year == 2017]["R_obs"].mean()
agg["delta_obs"]   = agg["R_obs"]   - R_2017
agg["delta_behav"] = agg["R_behav"] - R_2017
agg["delta_comp"]  = agg["R_comp"]  - R_2017

# Annual shares
agg["year"] = agg["month_dt"].dt.year
annual = (agg[agg["year"] >= 2018]
          .groupby("year")
          .apply(lambda x: pd.Series({
              "total":   x["delta_obs"].mean(),
              "behav":   x["delta_behav"].mean(),
              "comp":    x["delta_comp"].mean(),
          }))
          .reset_index())
annual["interaction"] = annual["total"] - annual["behav"] - annual["comp"]
annual["frac_behav"] = annual["behav"] / annual["total"]
annual["frac_comp"]  = annual["comp"]  / annual["total"]

print("\nShift-share decomposition:")
print(f"2017 baseline rate: {R_2017:.2f}%")
print(annual[["year","total","behav","comp","interaction","frac_behav","frac_comp"]].to_string(index=False, float_format=lambda x: f"{x:.3f}"))
print(f"Mean behavioral fraction 2018-2022: {annual['frac_behav'].mean():.1%}")
print(f"Mean compositional fraction 2018-2022: {annual['frac_comp'].mean():.1%}")

# Plot: left = time series counterfactuals; right = stacked bar decomposition
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11, 4.5))

# Left panel: time series
xs = np.array(range(len(agg)))
ys_obs   = lowess(agg["R_obs"].values,   xs, frac=0.3, return_sorted=False)
ys_behav = lowess(agg["R_behav"].values, xs, frac=0.3, return_sorted=False)
ys_comp  = lowess(agg["R_comp"].values,  xs, frac=0.3, return_sorted=False)

ax1.axhline(R_2017, color="gray", lw=1.2, ls="--", label="2017 baseline")
ax1.plot(agg["month_dt"], ys_obs,   color="black",   lw=2.2, label="Observed")
ax1.plot(agg["month_dt"], ys_behav, color="#d62728", lw=1.8, ls="--",
         label="Behavioral only (2017 composition)")
ax1.plot(agg["month_dt"], ys_comp,  color="#4878d0", lw=1.8, ls=":",
         label="Compositional only (2017 rates)")
ax1.set_xlabel("Month", fontsize=10)
ax1.set_ylabel("% very uncivil tweets", fontsize=10)
ax1.set_title("Observed vs counterfactual trends", fontsize=11)
ax1.legend(fontsize=8, loc="upper left")
ax1.grid(axis="y", alpha=0.3)
ax1.yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:.1f}%"))

# Right panel: stacked bars
bar_x = annual["year"].values
bar_behav = annual["behav"].values
bar_comp  = annual["comp"].values
bar_inter = annual["interaction"].values

bars_b = ax2.bar(bar_x, bar_behav, label="Behavioral", color="#d62728", alpha=0.85)
bars_c = ax2.bar(bar_x, bar_comp,  bottom=bar_behav, label="Compositional", color="#4878d0", alpha=0.85)
bot2 = bar_behav + bar_comp
bars_i = ax2.bar(bar_x, bar_inter, bottom=bot2, label="Interaction", color="#bdbdbd", alpha=0.85)

# Add percentage labels
for i, (yb, yc, yi, tot) in enumerate(zip(bar_behav, bar_comp, bar_inter, annual["total"].values)):
    if abs(tot) > 0.01:
        pct = yb / tot * 100
        ax2.text(bar_x[i], yb / 2, f"{pct:.0f}%", ha="center", va="center",
                 fontsize=9, color="white", fontweight="bold")

ax2.axhline(0, color="black", lw=0.8)
ax2.set_xlabel("Year", fontsize=10)
ax2.set_ylabel("Change from 2017 baseline (pp)", fontsize=10)
ax2.set_title("Annual shift-share decomposition", fontsize=11)
ax2.legend(fontsize=8, loc="upper left")
ax2.grid(axis="y", alpha=0.3)

fig.tight_layout()
fig.savefig(OUT / "fig_decomposition.pdf", bbox_inches="tight")
plt.close()
print("saved fig_decomposition.pdf")


# Score stacked (for SI)
# Check if we have y_any and n_tweets to compute the stacked bands
monthly_full = (df.groupby("month_dt")
                .apply(lambda x: pd.Series({
                    "rate_strong": x["y_strong"].sum() / x["n_tweets"].sum() * 100,
                    "rate_any":    x["y_any"].sum() / x["n_tweets"].sum() * 100,
                }))
                .reset_index())
monthly_full["rate_mild"] = monthly_full["rate_any"] - monthly_full["rate_strong"]
monthly_full["rate_civil"] = 100 - monthly_full["rate_any"]

print("\nAny incivility:")
print(f"Mean rate_any: {monthly_full['rate_any'].mean():.2f}%")
print(f"Mean rate_strong: {monthly_full['rate_strong'].mean():.2f}%")
print(f"First 3 months any: {monthly_full.head(3)['rate_any'].mean():.2f}%")
print(f"Last 3 months any:  {monthly_full.tail(3)['rate_any'].mean():.2f}%")

print("\nKey numbers for text:")
# Overall stats
print(f"Total party-month obs: {len(df)}")
n_parties = df['partyfactsid'].nunique()
n_countries = df['country'].nunique()
print(f"Unique parties: {n_parties}, Countries: {n_countries}")
print(f"Total tweets: {df['n_tweets'].sum():,.0f}")
total_strong = df['y_strong'].sum()
total_tweets = df['n_tweets'].sum()
total_any = df['y_any'].sum()
print(f"Overall strong incivility rate: {total_strong/total_tweets*100:.2f}%")
print(f"Overall any incivility rate:    {total_any/total_tweets*100:.2f}%")

print("\nDone.")
