# Codebook: de-identified THSS sleep–affect dataset

`thss_sleep_affect_deidentified.csv` is built by `R/06_make_shared_data.R` from the cleaned study files. It holds one row per youth per study day for the 54 youths in the analytic sample (953 youth-days). Missing values are blank.

**The file is not in this repository.** It will be deposited under controlled access once sharing is approved; see the Data Availability statement in the manuscript.

## What was removed or coarsened

- Study IDs are replaced by random IDs (1–54). The key linking them to study IDs is kept by the study team and is not shared.
- There are no calendar dates, only study day (1–21).
- Age is shared as two groups (14–15, 16–17) rather than in years.
- Race, ethnicity and all other demographics are omitted.
- Questionnaires are shared as totals only. No item-level CTQ, RFQ, loneliness or ACE responses are included.
- Biomarker data (CRP) and evening diaries are omitted.

## Variables

| Variable | Level | Description | Values |
|---|---|---|---|
| `id` | person | Random participant ID | 1–54 |
| `day` | day | Study day | 1–21 (days 1–7 baseline; 8–21 manipulation phase) |
| `condition` | person | Study condition | 0 = habitual sleep, 1 = sleep extension, 2 = not randomized (baseline week only) |
| `female` | person | Sex | 0 = male, 1 = female |
| `age_group` | person | Age group | `14-15`, `16-17` |
| `ACEs` | person | Adverse childhood experiences, parent report (count) | 0–9 |
| `CTQ` | person | Childhood Trauma Questionnaire total, 25 clinical items (reverse-scored items recoded; prorated when one item missing) | 25–125 possible |
| `RFQ` | person | Reflective Functioning Questionnaire total, 13 items (items 1, 3, 12 reversed; prorated when one item missing) | |
| `Loneliness` | person | UCLA Loneliness Scale total, 20 items (prorated when one item missing) | |
| `TST_hours` | day | Actigraphy total sleep time on the night before the morning report (hours) | |
| `vigorous_m` … `exhausted_m` | day | Morning affect items: how well each word described how the youth felt "this morning" | 0 = not at all to 4 = extremely |
| `PA` | day | Positive affect: mean of vigorous, cheerful, lively, enthusiastic, interested, content, grateful | 0–4 |
| `PA_high` | day | High-arousal PA: mean of enthusiastic, vigorous, cheerful, lively | 0–4 |
| `PA_low` | day | Low-arousal PA: mean of content, grateful | 0–4 |
| `NAff` | day | Negative affect: mean of sad, hopeless, discouraged, anxious, uneasy, on edge, angry, resentful, annoyed | 0–4 |
| `Irrit` | day | Irritability: mean of angry, annoyed, on edge, resentful | 0–4 |
| `Fatig` | day | Fatigue: mean of fatigued, worn out, exhausted | 0–4 |

## Reproducing the analyses

```bash
SHARED_FILE=/path/to/thss_sleep_affect_deidentified.csv Rscript R/00_from_shared.R
Rscript R/01_heterogeneity.R; Rscript R/02_sensitivity.R; Rscript R/03_moderators.R; Rscript R/05_figures.R
```

`R/00_from_shared.R` rebuilds the same analysis files as `R/00_clean_data.R`. Every day-level variable and every moderator matches the full pipeline exactly, except age: the age moderator uses the two age groups, so that one row of Table 4 will differ slightly from the published value. Table 1, the race and ethnicity table and the questionnaire reliabilities (`R/04_descriptives.R`) need the item-level and demographic data, so they can't be rebuilt from this file.
