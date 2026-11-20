# 06_make_shared_data.R — build the public de-identified analytic dataset (shared_data/).
# Run after 00_clean_data.R. The ID key and the re-identification check stay in data/ (git-ignored).
#   - one row per youth-day for the 54 analytic youths
#   - study IDs replaced by random IDs (1-54); the key is saved to data/derived/id_key.csv, which must not be shared
#   - study day (1-21) only, no calendar dates
#   - morning affect items and scale scores, nightly total sleep time, condition
#   - person-level questionnaire TOTALS only (ACEs, CTQ, RFQ, UCLA Loneliness); no item-level questionnaire data
#   - sex only; no age, race, ethnicity or other demographics
# Run it once: the random IDs differ on every run.
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(readr) })

daily  <- readRDS("data/derived/daily.rds")
person <- readRDS("data/derived/person_level.rds") %>% filter(analytic)

key <- tibble(ID = sample(person$ID), id = seq_len(nrow(person)))          # deliberately unseeded
items <- c("vigorous_m", "cheerful_m", "lively_m", "enthusiastic_m", "interested_m", "content_m", "grateful_m",
           "sad_m", "hopeless_m", "discouraged_m", "anxious_m", "uneasy_m", "on_edge_m", "angry_m", "resentful_m",
           "annoyed_m", "fatigued_m", "worn_out_m", "exhausted_m")
pers <- person %>% transmute(ID, female = Female, ACEs, CTQ, RFQ, Loneliness)
shared <- daily %>% filter(ID %in% person$ID) %>%
  select(ID, day = Study_day, condition = Group, TST_hours, all_of(items), PA, PA_high, PA_low, NAff, Irrit, Fatig) %>%
  left_join(pers, by = "ID") %>% left_join(key, by = "ID") %>%
  select(id, day, condition, female, ACEs, CTQ, RFQ, Loneliness, everything(), -ID) %>%
  arrange(id, day)

dir.create("shared_data", showWarnings = FALSE); dir.create("data/shared", recursive = TRUE, showWarnings = FALSE)
write_csv(shared, "shared_data/thss_sleep_affect_deidentified.csv", na = "")   # public copy (tracked)
write_csv(key, "data/derived/id_key.csv")

# re-identification check on the person-level quasi-identifiers that remain
p1 <- shared %>% group_by(id) %>% summarise(across(c(female, condition), ~ first(na.omit(.x))), .groups = "drop")
cells <- function(v) { t <- count(p1, across(all_of(v))); sprintf("%-32s min cell %d; cells of 1: %d; cells < 3: %d",
                                                                   paste(v, collapse = " x "), min(t$n), sum(t$n == 1), sum(t$n < 3)) }
risk <- c(sprintf("%d youths, %d youth-days", n_distinct(shared$id), nrow(shared)),
          cells("female"), cells("condition"), cells(c("female", "condition")))
writeLines(risk, "data/shared/reidentification_check.txt")
message(paste(risk, collapse = "\n"))
