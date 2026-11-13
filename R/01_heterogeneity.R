# 01_heterogeneity.R — average within-person associations (Table 2) and slope heterogeneity (Table 3)
source("R/functions.R")

res <- list()
for (lab in names(OUTCOMES)) {
  o <- OUTCOMES[[lab]]; d <- outcome_data(o); it <- iters(o, "het")
  fs <- fit_or_load(paste0("het_slope_", o), formula = y ~ TST_within + TST_between + (1 + TST_within | ID),
                    data = d, prior = PRIOR_SLOPE, chains = 4, iter = it[["iter"]], warmup = it[["warmup"]])
  fi <- fit_or_load(paste0("het_int_", o), formula = y ~ TST_within + TST_between + (1 | ID),
                    data = d, prior = PRIOR_INT, chains = 4, iter = it[["iter"]], warmup = it[["warmup"]])
  dr <- as_draws_df(fs); b <- dr$b_TST_within; s <- dr$sd_ID__TST_within
  ben <- if (o %in% BENEFIT_POSITIVE) pnorm(b / s) else pnorm(-b / s)
  ls <- loo(fs); li <- loo(fi); lc <- loo_compare(list(slope = ls, int = li))
  dl <- if (rownames(lc)[1] == "slope") c(-lc[2, "elpd_diff"], lc[2, "se_diff"]) else c(lc[2, "elpd_diff"], lc[2, "se_diff"])
  res[[lab]] <- tibble(outcome = lab, days = nrow(d), youths = n_distinct(d$ID),
    b_within = q3(b)[1], b_within_lo = q3(b)[2], b_within_hi = q3(b)[3],
    b_between = q3(dr$b_TST_between)[1], b_between_lo = q3(dr$b_TST_between)[2], b_between_hi = q3(dr$b_TST_between)[3],
    slope_sd = q3(s)[1], slope_sd_lo = q3(s)[2], slope_sd_hi = q3(s)[3],
    range_lo = mean(b) - 1.96 * mean(s), range_hi = mean(b) + 1.96 * mean(s),
    pct_benefit = q3(ben)[1], pct_benefit_lo = q3(ben)[2], pct_benefit_hi = q3(ben)[3],
    delta_elpd = dl[1], delta_elpd_se = dl[2],
    iter = it[["iter"]], warmup = it[["warmup"]], max_rhat = max(max_rhat(fs), max_rhat(fi)),
    divergences = n_div(fs) + n_div(fi),
    pareto_k_bad = sum(pareto_k_values(ls) > 0.7) + sum(pareto_k_values(li) > 0.7))
  message("heterogeneity done: ", lab)
}
res <- bind_rows(res)
write_csv(res, "output/heterogeneity_full.csv")

write_csv(res %>% transmute(Outcome = outcome, `Days / Youths` = paste(days, "/", youths),
  `TST within, b [95% CrI]` = mapply(function(a, b, c) ci(c(a, b, c)), b_within, b_within_lo, b_within_hi),
  `TST between, b [95% CrI]` = mapply(function(a, b, c) ci(c(a, b, c)), b_between, b_between_lo, b_between_hi)),
  "output/table2_associations.csv")

write_csv(res %>% transmute(Outcome = outcome,
  `Slope SD [95% CrI]` = mapply(function(a, b, c) ci(c(a, b, c)), slope_sd, slope_sd_lo, slope_sd_hi),
  `Implied range of individual slopes` = paste(fmt(range_lo), "to", fmt(range_hi)),
  `% with beneficial slope [95% CrI]` = sprintf("%.0f%% [%.0f%%, %.0f%%]", 100 * pct_benefit, 100 * pct_benefit_lo, 100 * pct_benefit_hi),
  `dELPD (SE)` = sprintf("%s (%s)", formatC(delta_elpd, format = "f", digits = 1), formatC(delta_elpd_se, format = "f", digits = 1))),
  "output/table3_heterogeneity.csv")
message("01_heterogeneity.R done")
