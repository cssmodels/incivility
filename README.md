# Elite Political Incivility Across Western Democracies

## Pipeline

The folders are numbered in the order the pipeline runs.

- `01_classification/` -- scores tweets for incivility (GPT-4.1, OpenAI Batch API, 0-3 ordinal scale) and assigns the 9-category topic classification.
- `02_validation/` -- draws the stratified human-coding samples and computes the LLM-vs-human agreement statistics (kappa) reported in the paper.
- `03_panel_construction/` -- aggregates scored tweets to party x country x month and party x country x month x topic panels, merging in V-Party, ParlGov, and V-Dem covariates.
- `04_analysis/` -- the hierarchical Bayesian beta-binomial models: the main model, the robustness checks (any-incivility threshold, Perspective API, populism interactions, liberal-democracy backsliding trend), and the party-family model.
- `05_figures/` -- descriptive figures and the shift-share decomposition.

Each script documents its own inputs, outputs, and command-line usage in its docstring. Within `04_analysis/`, `analysis_incivility.py` is the main entry point; `refit_partyfamily_simple.py` and `run_any_threshold.sh` each run one specific robustness variant on top of it.

## Data

This repository contains code only.

The aggregated party-country-month panel data will be made publicly available upon acceptance. Tweet IDs for the full corpus are available upon request, subject to Twitter/X terms of service.

Scripts in `01_classification/` and `03_panel_construction/` expect the raw tweet-level parquet files; scripts in `02_validation/` additionally expect the human-coded validation samples. Paths marked `/path/to/...` or `/Volumes/...` in those scripts are placeholders for your own copies of this data -- see each script's docstring for the exact files and columns expected.

## Requirements

See `requirements.txt`. Model fitting in `04_analysis/` uses PyMC with the `numpyro`/JAX sampler backend; the main model and robustness checks were fit on a GPU and take roughly 2-3 hours each.

## License

MIT. See `LICENSE`.
