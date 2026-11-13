# 00_clean_data.R
# Builds the analytic daily file and the corrected person-level scores from the raw THSS exports.
# Raw files are read from DATA_DIR (default: data/raw) and are never modified.
# Outputs (git-ignored, participant data): data/derived/daily.rds, data/derived/person_level.csv
#
# Corrections relative to the scores stored in the raw exports:
#   * Negative missing-value codes (e.g., -99) in questionnaire items are set to NA.
#   * CTQ total = sum of the 25 clinical items (minimization items 10, 16, 22 excluded), with items
#     2, 5, 7, 13, 19, 26, 28 reverse-scored from the raw items (6 - x). Prorated (mean x 25) when one
#     item is missing; NA when more than one is missing. The stored CTQ_Sum treated missing items as 0.
#   * RFQ total = sum of all 13 items with items 1, 3, 12 reverse-scored (6 - x), prorated the same way.
#     The stored RFQ_Sum ran one point low and treated missing items as 0.
#   * UCLA Loneliness = sum of the 20 items (0-3), prorated the same way.
#   * Person-level variables (ACEs, age, gender, condition) are taken as each youth's first non-missing
#     value across all rows. In the raw .sav, ACEs for ID 2109 is stored on only one of that youth's rows.

suppressPackageStartupMessages({ library(haven); library(dplyr); library(readr) })

DATA_DIR <- Sys.getenv("DATA_DIR", "data/raw")
OUT_DIR  <- "data/derived"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

num  <- function(x) { v <- suppressWarnings(as.numeric(x)); v[!is.na(v) & v < 0] <- NA; v }
fnn  <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_ }
prorate <- function(M) { k <- ncol(M); n <- rowSums(!is.na(M)); ifelse(n >= k - 1, rowMeans(M, na.rm = TRUE) * k, NA_real_) }

# ---------- raw files ----------
sav <- read_sav(file.path(DATA_DIR, "THSS Daily Sleep and Diary Data.sav")) %>%
  mutate(ID = as.numeric(id), Study_day = as.numeric(Study_Day))
csv <- as.data.frame(read_csv(file.path(DATA_DIR, "THSS LONG CRP and Affect 6.30.2026.csv"),
                              col_types = cols(.default = "c"), locale = locale(encoding = "latin1"),
                              progress = FALSE))
names(csv)[1] <- "ID"                                  # first column carries a byte-order mark
csv$ID <- suppressWarnings(as.numeric(csv$ID))
csv$Study_day <- suppressWarnings(as.numeric(csv$Study_day))

# ---------- daily affect items (morning report) ----------
pa_items   <- c("vigorous_m", "cheerful_m", "lively_m", "enthusiastic_m", "interested_m", "content_m", "grateful_m")
na_items   <- c("sad_m", "hopeless_m", "discouraged_m", "anxious_m", "uneasy_m", "on_edge_m", "angry_m", "resentful_m", "annoyed_m")
hipa_items <- c("enthusiastic_m", "vigorous_m", "cheerful_m", "lively_m")
lopa_items <- c("content_m", "grateful_m")
irr_items  <- c("angry_m", "annoyed_m", "on_edge_m", "resentful_m")
fat_items  <- c("fatigued_m", "worn_out_m", "exhausted_m")
items <- unique(c(pa_items, na_items, fat_items))

daily_items <- csv %>% filter(!is.na(ID), !is.na(Study_day)) %>%
  transmute(ID, Study_day, across(all_of(items), num)) %>%
  distinct(ID, Study_day, .keep_all = TRUE)

daily <- sav %>%
  transmute(ID, Study_day, TST_hours = as.numeric(NumTST), PA_stored = as.numeric(PA_m), NA_stored = as.numeric(NA_m),
            Group = as.numeric(Group)) %>%
  left_join(daily_items, by = c("ID", "Study_day")) %>%
  mutate(PA      = rowMeans(across(all_of(pa_items))),
         NAff    = rowMeans(across(all_of(na_items))),
         PA_high = rowMeans(across(all_of(hipa_items))),
         PA_low  = rowMeans(across(all_of(lopa_items))),
         Irrit   = rowMeans(across(all_of(irr_items))),
         Fatig   = rowMeans(across(all_of(fat_items))))

# composites rebuilt from items must equal the stored PA_m / NA_m
chk <- with(daily, c(PA = max(abs(PA - PA_stored), na.rm = TRUE), NAff = max(abs(NAff - NA_stored), na.rm = TRUE)))
if (any(chk > 1e-8)) stop("Rebuilt composites differ from stored PA_m/NA_m: ", paste(round(chk, 4), collapse = ", "))
daily <- daily %>% mutate(PA = coalesce(PA, PA_stored), NAff = coalesce(NAff, NA_stored)) %>% select(-PA_stored, -NA_stored)

# ---------- person-level scores ----------
ctq_items <- setdiff(paste0("ctq", 1:28), paste0("ctq", c(10, 16, 22)))
ctq_rev   <- paste0("ctq", c(2, 5, 7, 13, 19, 26, 28))
rfq_items <- paste0("rfq", 1:13, "a"); rfq_rev <- paste0("rfq", c(1, 3, 12), "a")
lon_items <- paste0("loneliness", 1:20, "a")

q <- csv %>% filter(!is.na(ID)) %>% transmute(ID, across(all_of(c(ctq_items, rfq_items, lon_items)), num)) %>%
  group_by(ID) %>% summarise(across(everything(), fnn), .groups = "drop")
q[ctq_rev] <- 6 - q[ctq_rev]
q[rfq_rev] <- 6 - q[rfq_rev]
scores <- tibble(ID = q$ID,
                 CTQ = prorate(as.matrix(q[ctq_items])), CTQ_missing_items = rowSums(is.na(q[ctq_items])),
                 RFQ = prorate(as.matrix(q[rfq_items])), RFQ_missing_items = rowSums(is.na(q[rfq_items])),
                 Loneliness = prorate(as.matrix(q[lon_items])), Lon_missing_items = rowSums(is.na(q[lon_items])))
scores$CTQ[rowSums(!is.na(q[ctq_items])) == 0] <- NA
scores$RFQ[rowSums(!is.na(q[rfq_items])) == 0] <- NA
scores$Loneliness[rowSums(!is.na(q[lon_items])) == 0] <- NA

demo <- sav %>% group_by(ID) %>%
  summarise(ACEs = fnn(as.numeric(ACES)), Age = fnn(as.numeric(age)), gender = fnn(as.numeric(gender)),
            Group = fnn(as.numeric(Group)), .groups = "drop")
race <- csv %>% filter(!is.na(ID)) %>% transmute(ID, hispanic = num(hispanic), across(paste0("race___", 1:8), num)) %>%
  group_by(ID) %>% summarise(across(everything(), fnn), .groups = "drop")

person <- demo %>% left_join(scores, by = "ID") %>% left_join(race, by = "ID") %>%
  mutate(Female = as.numeric(gender == 2))              # REDCap: 1 = Male, 2 = Female

# ---------- analytic sample: >= 1 day with TST and the morning PA report ----------
counts <- daily %>% group_by(ID) %>% summarise(days_both = sum(!is.na(TST_hours) & !is.na(PA)), .groups = "drop")
person <- person %>% left_join(counts, by = "ID") %>% mutate(analytic = days_both > 0)

# ---------- within/between split of TST (person mean over all nights with TST) ----------
daily <- daily %>% semi_join(filter(person, analytic), by = "ID") %>%
  group_by(ID) %>% mutate(TST_pm = mean(TST_hours, na.rm = TRUE), TST_within = TST_hours - TST_pm) %>% ungroup() %>%
  mutate(TST_between = TST_pm - mean(TST_pm, na.rm = TRUE))   # grand-mean centred over day rows, as in the original models

# standardize continuous moderators across analytic youths
an <- filter(person, analytic)
z <- function(v) (v - mean(an[[v]], na.rm = TRUE)) / sd(an[[v]], na.rm = TRUE)
person <- person %>% mutate(ACEs_z = (ACEs - mean(an$ACEs, na.rm = TRUE)) / sd(an$ACEs, na.rm = TRUE),
                            CTQ_z = (CTQ - mean(an$CTQ, na.rm = TRUE)) / sd(an$CTQ, na.rm = TRUE),
                            RFQ_z = (RFQ - mean(an$RFQ, na.rm = TRUE)) / sd(an$RFQ, na.rm = TRUE),
                            Lon_z = (Loneliness - mean(an$Loneliness, na.rm = TRUE)) / sd(an$Loneliness, na.rm = TRUE),
                            Age_z = (Age - mean(an$Age, na.rm = TRUE)) / sd(an$Age, na.rm = TRUE))

saveRDS(daily, file.path(OUT_DIR, "daily.rds"))
write_csv(person, file.path(OUT_DIR, "person_level.csv"))
saveRDS(person, file.path(OUT_DIR, "person_level.rds"))

# correspondence with stored totals (aggregate only; no participant values are written to output/)
stored <- sav %>% group_by(ID) %>% summarise(CTQ_stored = fnn(as.numeric(CTQ_Sum)), RFQ_stored = fnn(as.numeric(RFQ_Sum)),
                                             Lon_stored = fnn(as.numeric(Loneline)), .groups = "drop")
cmp <- an %>% left_join(stored, by = "ID")
dir.create("output", showWarnings = FALSE)
writeLines(c(
  sprintf("Youths in raw file: %d; analytic (>=1 day with TST and morning PA): %d", nrow(person), sum(person$analytic, na.rm = TRUE)),
  sprintf("Corrected vs stored totals (analytic youths): r(CTQ) = %.3f, r(RFQ) = %.3f, r(Loneliness) = %.3f",
          cor(cmp$CTQ, cmp$CTQ_stored, use = "complete"), cor(cmp$RFQ, cmp$RFQ_stored, use = "complete"),
          cor(cmp$Loneliness, cmp$Lon_stored, use = "complete")),
  sprintf("Youths with a prorated total: CTQ %d, RFQ %d, Loneliness %d",
          sum(an$CTQ_missing_items == 1, na.rm = TRUE), sum(an$RFQ_missing_items == 1, na.rm = TRUE), sum(an$Lon_missing_items == 1, na.rm = TRUE)),
  sprintf("Youths whose ACEs appear on only some of their rows (filled from first non-missing): %d",
          sum(sav %>% group_by(ID) %>% summarise(k = any(is.na(as.numeric(ACES))) & any(!is.na(as.numeric(ACES)))) %>% pull(k)))
), "output/00_cleaning_log.txt")
message("00_clean_data.R done")
