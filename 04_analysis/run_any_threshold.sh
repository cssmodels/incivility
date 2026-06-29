#!/usr/bin/env bash
# Runs the main beta-binomial model with DV = y_any (incivility >= 1), the
# "any incivility" threshold robustness check reported in the Supplementary
# Information. Results go to results_v4_yany/, compared there against
# results_v3/ (y_strong, severe incivility, >= 2) without overwriting either.
#
# Usage (from the GPU server, with the data in place):
#   bash run_any_threshold.sh
#
# Estimated runtime: ~2-3 h, same order as the main model.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
DATA="${REPO_ROOT}/2026aggregated/party_country_month_panel_v4.df.pkl"
OUTDIR="${REPO_ROOT}/results_v4_yany"

echo "Any-incivility threshold robustness model"
echo "DV: y_any (tweets scoring >= 1)"
echo "Output: ${OUTDIR}"
echo "Started: $(date)"

python "${SCRIPT_DIR}/analysis_incivility.py" \
    --data    "${DATA}" \
    --outdir  "${OUTDIR}" \
    --dv      y_any \
    --draws   2000 \
    --tune    2000 \
    --chains  4 \
    --target-accept 0.99 \
    --skip-pop-opp \
    --skip-pop-libdem \
    --skip-pop-time \
    --skip-libdem-trend \
    --skip-persp \
    --skip-topic

echo "Completed: $(date)"
echo "Results in: ${OUTDIR}"
