# 06_make_shared_data.R — build the de-identified analytic dataset for controlled-access sharing.
# Run after 00_clean_data.R. Writes to data/shared/ (git-ignored); nothing here is published automatically.
#   - one row per youth-day for the 54 analytic youths
#   - study IDs replaced by random IDs (1-54); the key is saved to data/derived/id_key.csv, which must not be shared
#   - study day (1-21) only, no calendar dates
#   - morning affect items and scale scores, nightly total sleep time, condition
#   - person-level questionnaire TOTALS only (ACEs, CTQ, RFQ, UCLA Loneliness); no item-level questionnaire data
#   - sex; age coarsened to 14-15 vs 16-17; no race, ethnicity or other demographics
# Run it once: the random IDs differ on every run.
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(readr) })

daily  <- readRDS("data/derived/daily.rds")
person <- readRDS("data/derived/person_level.rds") %>% filter(analytic)

key <- tibble(ID = sample(person$ID), id = seq_len(nrow(person)))          # deliberately unseeded
items <- c("vigorous_m", "cheerful_m", "lively_m", "enthusiastic_m", "interested_m", "content_m", "grateful_m",
           "sad_m", "hopeless_m", "discouraged_m", "anxious_m", "uneasy_m", "on_edge_m", "angry_m", "resentful_m",
           "annoyed_m", "fatigued_m", "worn_out_m", "exhausted_m")
pers <- person %>% transmute(ID, female = Female,
                             age_group = case_when(Age <= 15 ~ "14-15", Age >= 16 ~ "16-17"),
                             ACEs, CTQ, RFQ, Loneliness)
shared <- daily %>% filter(ID %in% person$ID) %>%
  select(ID, day = Study_day, condition = Group, TST_hours, all_of(items), PA, PA_high, PA_low, NAff, Irrit, Fatig) %>%
  left_join(pers, by = "ID") %>% left_join(key, by = "ID") %>%
  select(id, day, condition, female, age_group, ACEs, CTQ, RFQ, Loneliness, everything(), -ID) %>%
  arrange(id, day)

dir.create("data/shared", recursive = TRUE, showWarnings = FALSE)
write_csv(shared, "data/shared/thss_sleep_affect_deidentified.csv", na = "")
write_csv(key, "data/derived/id_key.csv")

# re-identification check on the person-level quasi-identifiers that remain
p1 <- shared %>% group_by(id) %>% summarise(across(c(female, age_group, condition), ~ first(na.omit(.x))), .groups = "drop")
cells <- function(v) { t <- count(p1, across(all_of(v))); sprintf("%-32s min cell %d; cells of 1: %d; cells < 3: %d",
                                                                   paste(v, collapse = " x "), min(t$n), sum(t$n == 1), sum(t$n < 3)) }
risk <- c(sprintf("%d youths, %d youth-days", n_distinct(shared$id), nrow(shared)),
          cells(c("female", "age_group")), cells(c("female", "condition")), cells(c("age_group", "condition")),
          cells(c("female", "age_group", "condition")))
writeLines(risk, "data/shared/reidentification_check.txt")
message(paste(risk, collapse = "\n"))
