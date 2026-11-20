# 00_from_shared.R — rebuild the analysis files from the de-identified shared dataset instead of the raw exports.
#   SHARED_FILE=/path/to/thss_sleep_affect_deidentified.csv Rscript R/00_from_shared.R
# Then run R/01_heterogeneity.R, R/02_sensitivity.R, R/03_moderators.R and R/05_figures.R as usual.
# Differences from the full pipeline: age is shared as two groups (14-15, 16-17), so the age moderator uses that
# grouping instead of exact age; 04_descriptives.R needs the raw exports (race/ethnicity, questionnaire items).
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(readr) })

s <- read_csv(Sys.getenv("SHARED_FILE", "data/shared/thss_sleep_affect_deidentified.csv"), show_col_types = FALSE)

daily <- s %>% transmute(ID = id, Study_day = day, TST_hours, Group = condition,
                         across(vigorous_m:exhausted_m), PA, NAff, PA_high, PA_low, Irrit, Fatig) %>%
  group_by(ID) %>% mutate(TST_pm = mean(TST_hours, na.rm = TRUE), TST_within = TST_hours - TST_pm) %>% ungroup() %>%
  mutate(TST_between = TST_pm - mean(TST_pm, na.rm = TRUE))   # grand-mean centred over day rows, as in 00_clean_data.R

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
fnn <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA }   # first non-missing value per youth
person <- s %>% group_by(id) %>% summarise(across(c(ACEs, CTQ, RFQ, Loneliness, female, condition, age_group), fnn), .groups = "drop") %>%
  transmute(ID = id, ACEs, CTQ, RFQ, Loneliness, Female = female, Group = condition,
            Age = ifelse(is.na(age_group), NA_real_, as.numeric(age_group == "16-17")), analytic = TRUE) %>%
  mutate(ACEs_z = z(ACEs), CTQ_z = z(CTQ), RFQ_z = z(RFQ), Lon_z = z(Loneliness), Age_z = z(Age))

OUT_DIR <- Sys.getenv("DERIVED_DIR", "data/derived"); dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
saveRDS(daily, file.path(OUT_DIR, "daily.rds"))
saveRDS(person, file.path(OUT_DIR, "person_level.rds"))
message("00_from_shared.R done: ", n_distinct(daily$ID), " youths, ", nrow(daily), " youth-days")
