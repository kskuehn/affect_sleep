# 03_moderators.R — cross-level interactions (Table 4): 6 outcomes x 6 moderators, one model each.
# Every outcome uses the same analytic days as in 01_heterogeneity.R (full sample, not a CRP subsample).
# Continuous moderators are standardized across analytic youths; Female is 0 = male, 1 = female.
source("R/functions.R")

MODS <- c(ACEs = "ACEs_z", CTQ = "CTQ_z", RFQ = "RFQ_z", Loneliness = "Lon_z", Age = "Age_z", Sex = "Female")
res <- list()
for (lab in names(OUTCOMES)) for (m in names(MODS)) {
  o <- OUTCOMES[[lab]]; it <- iters(o, "mod")
  d <- outcome_data(o) %>% left_join(person %>% select(ID, M = all_of(MODS[[m]])), by = "ID") %>% filter(!is.na(M))
  f <- fit_or_load(paste0("mod_", o, "_x_", m), formula = y ~ TST_within * M + TST_between + (1 + TST_within | ID),
                   data = d, prior = PRIOR_SLOPE, chains = 4, iter = it[["iter"]], warmup = it[["warmup"]])
  dr <- as_draws_df(f); bi <- dr$`b_TST_within:M`; bw <- dr$b_TST_within
  lv <- if (m == "Sex") c(0, 1) else c(-1, 1)
  lo <- bw + bi * lv[1]; hi <- bw + bi * lv[2]
  res[[paste(lab, m)]] <- tibble(outcome = lab, moderator = m, days = nrow(d), youths = n_distinct(d$ID),
    b_int = q3(bi)[1], b_int_lo = q3(bi)[2], b_int_hi = q3(bi)[3],
    credible = q3(bi)[2] > 0 | q3(bi)[3] < 0,
    slope_low = q3(lo)[1], slope_low_lo = q3(lo)[2], slope_low_hi = q3(lo)[3],
    slope_high = q3(hi)[1], slope_high_lo = q3(hi)[2], slope_high_hi = q3(hi)[3],
    max_rhat = max_rhat(f), divergences = n_div(f))
  message("moderator done: ", lab, " x ", m)
}
res <- bind_rows(res)
write_csv(res, "output/moderators_full.csv")

wide <- res %>% mutate(cell = paste0(mapply(function(a, b, c) ci(c(a, b, c)), b_int, b_int_lo, b_int_hi), ifelse(credible, "*", ""))) %>%
  select(outcome, moderator, cell) %>% pivot_wider(names_from = moderator, values_from = cell) %>%
  slice(match(names(OUTCOMES), outcome)) %>% rename(Outcome = outcome)
write_csv(wide, "output/table4_moderators.csv")
message("03_moderators.R done; credible interactions: ", sum(res$credible))
