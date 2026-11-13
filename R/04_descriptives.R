# 04_descriptives.R — Table 1, missing-data checks, reliability of affect scales and questionnaires
source("R/functions.R")
suppressPackageStartupMessages({ library(lme4); library(psych) })

an <- person %>% filter(analytic)
msd <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
rngs <- function(x) sprintf("%s\u2013%s", formatC(min(x, na.rm = TRUE), format = "fg"), formatC(max(x, na.rm = TRUE), format = "fg"))
cnt <- function(x) sum(!is.na(x))
cond_lab <- c("0" = "Habitual sleep", "1" = "Sleep extension", "2" = "Not randomized")

t1_person <- bind_rows(
  tibble(Characteristic = "Age (years)", n = cnt(an$Age), Value = msd(an$Age), Range = rngs(an$Age)),
  tibble(Characteristic = "Female", n = cnt(an$Female), Value = sprintf("%d (%.1f%%)", sum(an$Female == 1, na.rm = TRUE), 100 * mean(an$Female, na.rm = TRUE)), Range = ""),
  an %>% count(Group, name = "k") %>% transmute(Characteristic = cond_lab[as.character(Group)], n = NA_integer_,
                                    Value = sprintf("%d (%.1f%%)", k, 100 * k / nrow(an)), Range = ""),
  tibble(Characteristic = c("ACEs", "CTQ total", "RFQ total", "Loneliness"),
         n = c(cnt(an$ACEs), cnt(an$CTQ), cnt(an$RFQ), cnt(an$Loneliness)),
         Value = c(msd(an$ACEs), msd(an$CTQ), msd(an$RFQ), msd(an$Loneliness)),
         Range = c(rngs(an$ACEs), rngs(an$CTQ), rngs(an$RFQ), rngs(an$Loneliness))))
write_csv(t1_person, "output/table1_person_level.csv")

daily_desc <- function(v, lab) {
  d <- daily %>% filter(!is.na(.data[[v]])) %>% transmute(ID, y = .data[[v]])
  m <- lmer(y ~ 1 + (1 | ID), data = d); vc <- as.data.frame(VarCorr(m))
  pm <- d %>% group_by(ID) %>% summarise(m = mean(y), .groups = "drop")
  tibble(Variable = lab, Days = nrow(d), Youths = n_distinct(d$ID), M = mean(d$y),
         SD_between = sd(pm$m), SD_within = sd(d$y - pm$m[match(d$ID, pm$ID)]),
         ICC = vc$vcov[1] / sum(vc$vcov), Range = rngs(round(d$y, 2)))
}
t1_daily <- bind_rows(c(list(daily_desc("TST_hours", "Total sleep time (h)")),
                        unname(Map(daily_desc, unname(OUTCOMES), names(OUTCOMES)))))
write_csv(t1_daily %>% mutate(across(c(M, SD_between, SD_within), ~ round(.x, 2)), ICC = round(ICC, 2)),
          "output/table1_daily_level.csv")

# race / ethnicity (analytic youths with demographic data)
race_lab <- c("Asian or Pacific Islander", "Black or African American", "Hispanic / Spanish origin", "Latino / Latinx",
              "Middle Eastern or North African", "Native American", "White", "Other")
nd <- sum(!is.na(an$race___7))
race_tab <- tibble(Category = c(race_lab, "Hispanic or Latino (yes/no item)"),
                   n = c(sapply(1:8, function(k) sum(an[[paste0("race___", k)]] == 1, na.rm = TRUE)), sum(an$hispanic == 1, na.rm = TRUE)),
                   of = c(rep(nd, 8), sum(!is.na(an$hispanic)))) %>% mutate(pct = round(100 * n / of, 1))
write_csv(race_tab, "output/race_ethnicity.csv")

# missing data
dd <- daily %>% mutate(both = !is.na(TST_hours) & !is.na(PA), miss_aff = is.na(PA))
poss <- dd %>% group_by(ID) %>% summarise(Group = first(Group), possible = ifelse(first(Group) == 2, 7, 21),
                                          both = sum(both), aff = sum(!miss_aff), tst = sum(!is.na(TST_hours)), .groups = "drop") %>%
  left_join(person %>% select(ID, ACEs, CTQ, RFQ, Loneliness, Age, Female), by = "ID")
gm <- glmer(miss_aff ~ TST_within + TST_between + (1 | ID), data = filter(dd, !is.na(TST_hours)), family = binomial)
cf <- summary(gm)$coefficients
rz <- poss %>% filter(Group != 2)
cors <- sapply(c("ACEs", "CTQ", "RFQ", "Loneliness", "Age"), function(v) { t <- cor.test(rz$both, rz[[v]]); c(r = unname(t$estimate), p = t$p.value) })
tt <- t.test(both ~ Group, data = rz)
miss_lines <- c(
  sprintf("Days with both measures: %d of %d possible (%.1f%%); affect %d; actigraphy %d", sum(poss$both), sum(poss$possible),
          100 * sum(poss$both) / sum(poss$possible), sum(poss$aff), sum(poss$tst)),
  sprintf("Days per youth: M = %.1f, SD = %.2f, range %d-%d", mean(poss$both), sd(poss$both), min(poss$both), max(poss$both)),
  sprintf("Randomized: M = %.1f of 21 (range %d-%d); not randomized: M = %.1f of 7 (range %d-%d)",
          mean(rz$both), min(rz$both), max(rz$both), mean(poss$both[poss$Group == 2]), min(poss$both[poss$Group == 2]), max(poss$both[poss$Group == 2])),
  sprintf("Missed morning report ~ within-person TST: OR = %.2f, 95%% CI [%.2f, %.2f], p = %.2f",
          exp(cf[2, 1]), exp(cf[2, 1] - 1.96 * cf[2, 2]), exp(cf[2, 1] + 1.96 * cf[2, 2]), cf[2, 4]),
  sprintf("Completed days by condition (randomized): %s, p = %.2f", paste(round(tt$estimate, 1), collapse = " vs "), tt$p.value),
  paste0("Completed days ~ person-level (randomized): ", paste(sprintf("%s r = %.2f (p = %.2f)", colnames(cors), cors["r", ], cors["p", ]), collapse = "; ")))
writeLines(miss_lines, "output/missing_data.txt")

# reliability: within-person (Cranford et al., 2006) for morning scales; alpha for questionnaires
raw <- as.data.frame(readr::read_csv(file.path(Sys.getenv("DATA_DIR", "data/raw"), "THSS LONG CRP and Affect 6.30.2026.csv"),
                                     col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = "latin1"), progress = FALSE))
names(raw)[1] <- "ID"
num <- function(x) { v <- suppressWarnings(as.numeric(x)); v[!is.na(v) & v < 0] <- NA; v }
scales <- list(`Positive affect` = c("vigorous_m", "cheerful_m", "lively_m", "enthusiastic_m", "interested_m", "content_m", "grateful_m"),
               `Negative affect` = c("sad_m", "hopeless_m", "discouraged_m", "anxious_m", "uneasy_m", "on_edge_m", "angry_m", "resentful_m", "annoyed_m"),
               `High-arousal PA` = c("enthusiastic_m", "vigorous_m", "cheerful_m", "lively_m"), `Low-arousal PA` = c("content_m", "grateful_m"),
               Irritability = c("angry_m", "annoyed_m", "on_edge_m", "resentful_m"), Fatigue = c("fatigued_m", "worn_out_m", "exhausted_m"))
rr <- raw %>% mutate(ID = num(ID), day = num(Study_day)) %>% filter(ID %in% an$ID, !is.na(day))
rel <- bind_rows(lapply(names(scales), function(nm) {
  x <- rr[, c("ID", "day", scales[[nm]])]; x[scales[[nm]]] <- lapply(x[scales[[nm]]], num); x <- x[complete.cases(x), ]
  m <- psych::multilevel.reliability(x, grp = "ID", Time = "day", items = scales[[nm]], lme = FALSE, lmer = TRUE,
                                     plot = FALSE, alpha = FALSE, long = FALSE, aov = FALSE)
  tibble(Scale = nm, items = length(scales[[nm]]), RkF = round(m$RkF, 2), Rc = round(m$Rc, 2))
}))
first_row <- rr %>% filter(!is.na(num(ctq1))) %>% group_by(ID) %>% slice(1) %>% ungroup()
item_mat <- function(v, rev = character()) { X <- as.data.frame(sapply(v, function(i) num(first_row[[i]]))); X[rev] <- 6 - X[rev]; X }
alpha_of <- function(X) round(psych::alpha(X, warnings = FALSE, use = "pairwise")$total$raw_alpha, 2)
ctq_items <- setdiff(paste0("ctq", 1:28), paste0("ctq", c(10, 16, 22)))
rel <- bind_rows(rel, tibble(Scale = c("CTQ", "RFQ", "UCLA Loneliness"), items = c(25, 13, 20),
  alpha = c(alpha_of(item_mat(ctq_items, paste0("ctq", c(2, 5, 7, 13, 19, 26, 28)))),
            alpha_of(item_mat(paste0("rfq", 1:13, "a"), paste0("rfq", c(1, 3, 12), "a"))),
            alpha_of(item_mat(paste0("loneliness", 1:20, "a"))))))
write_csv(rel, "output/reliability.csv")
message("04_descriptives.R done")
