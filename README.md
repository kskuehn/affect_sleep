# Person-specific associations between sleep and affect

Analysis code for *Person-specific associations between sleep and affect* (Kuehn, Antici, Mousavi-Raad, … & Kuhlman), a secondary analysis of the UCI Teen Healthy Sleep Study (Mousavi-Raad et al., 2026, *Annals of Behavioral Medicine*).

The pipeline rebuilds every number in the manuscript's tables and Results from the two raw study exports.

## Data

The raw data are **not** in this repository. Put the two exports in `data/raw/` (or point `DATA_DIR` at them):

- `THSS Daily Sleep and Diary Data.sav`
- `THSS LONG CRP and Affect 6.30.2026.csv`

Everything under `data/` is git-ignored, including the cleaned files and fitted models the pipeline writes.

## Running

```bash
DATA_DIR=/path/to/raw Rscript R/run_all.R
```

Requires R with `brms`, `rstan`, `loo`, `dplyr`, `tidyr`, `readr`, `haven`, `lme4` and `psych`. The full run fits 54 models and takes about an hour on four cores. Fitted models are cached in `data/fits/`; delete a file there to refit that model.

| Script | What it does | Writes |
|---|---|---|
| `R/00_clean_data.R` | Builds the analytic daily file and corrected person-level scores | `data/derived/` (ignored), `output/00_cleaning_log.txt` |
| `R/04_descriptives.R` | Table 1, race/ethnicity, missing-data checks, reliability | `output/table1_*.csv`, `race_ethnicity.csv`, `missing_data.txt`, `reliability.csv` |
| `R/01_heterogeneity.R` | Average within-person associations and random-slope heterogeneity, with LOO comparisons | `output/table2_associations.csv`, `table3_heterogeneity.csv`, `heterogeneity_full.csv` |
| `R/02_sensitivity.R` | Average associations adjusted for study day and phase × condition | `output/sensitivity_condition_time.csv` |
| `R/03_moderators.R` | 36 cross-level interactions (6 outcomes × 6 moderators) | `output/table4_moderators.csv`, `moderators_full.csv` |

## Analytic decisions

- **Sample.** Every youth with at least one day with both actigraphy and the morning affect report (N = 54), including the 12 youths who completed the baseline week but were not randomized.
- **Outcomes.** Morning affect (0–4). Composite PA = mean of 7 items (vigorous, cheerful, lively, enthusiastic, interested, content, grateful); composite NA = mean of 9 items (sad, hopeless, discouraged, anxious, uneasy, on edge, angry, resentful, annoyed). Subscales: high-arousal PA (enthusiastic, vigorous, cheerful, lively), low-arousal PA (content, grateful), irritability (angry, annoyed, on edge, resentful), fatigue (fatigued, worn out, exhausted; not part of the NA composite). In subscale models a day is kept only when both subscales of its pair are observed.
- **Sleep.** Actigraphy total sleep time, split into a within-person deviation from the youth's mean and a grand-mean-centered between-person mean.
- **Models.** Gaussian multilevel models in `brms` with random intercepts and random within-person slopes; priors N(0, 5) intercept, N(0, 2) coefficients, half-t(3, 0, 2) SDs, LKJ(2). Four chains; 3,000 iterations (1,000 warmup) for association models and 4,000 (1,500 warmup) for moderator models. Low-arousal PA models are run twice as long to reach R̂ ≤ 1.01.
- **Moderators.** ACEs, CTQ, RFQ, loneliness and age standardized across analytic youths; sex coded 0 = male, 1 = female (REDCap gender: 1 = male, 2 = female).

## Corrections to the stored scores

`R/00_clean_data.R` rescores the questionnaires from item-level data instead of using the totals stored in the exports:

- Negative missing-value codes (−99) are set to missing. Two youths had −99 in one CTQ item, which the stored `CTQ_Sum` counted as 0.
- The RFQ total uses all 13 items, with items 1, 3 and 12 reverse-scored. The stored `RFQ_Sum` ran one point low and counted missing items as 0.
- CTQ, RFQ and UCLA Loneliness totals are prorated when one item is missing.
- Person-level variables use each youth's first non-missing value; ACEs for one youth was stored on only one of that youth's rows.

The corrected totals correlate ≥ .997 with the stored ones.
