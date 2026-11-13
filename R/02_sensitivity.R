# 02_sensitivity.R — average associations adjusted for study day and phase x condition
# Phase: baseline week (days 1-7) vs intervention weeks (days 8-21). Condition: habitual, extension,
# not randomized. Youths who were not randomized contribute baseline days only.
source("R/functions.R")

res <- list()
for (lab in names(OUTCOMES)) {
  o <- OUTCOMES[[lab]]; it <- iters(o, "het")
  d <- outcome_data(o) %>% mutate(phase = as.numeric(Study_day > 7), cond = factor(Group), day_c = (Study_day - 11) / 7)
  f <- fit_or_load(paste0("sens_", o), formula = y ~ TST_within + TST_between + day_c + phase * cond + (1 + TST_within | ID),
                   data = d, prior = PRIOR_SLOPE, chains = 4, iter = it[["iter"]], warmup = it[["warmup"]])
  dr <- as_draws_df(f)
  res[[lab]] <- tibble(outcome = lab, days = nrow(d),
    b_within = q3(dr$b_TST_within)[1], b_within_lo = q3(dr$b_TST_within)[2], b_within_hi = q3(dr$b_TST_within)[3],
    slope_sd = q3(dr$sd_ID__TST_within)[1], slope_sd_lo = q3(dr$sd_ID__TST_within)[2], slope_sd_hi = q3(dr$sd_ID__TST_within)[3],
    iter = fit_iter(f), max_rhat = max_rhat(f), divergences = n_div(f))
  message("sensitivity done: ", lab)
}
res <- bind_rows(res)
write_csv(res, "output/sensitivity_condition_time.csv")
message("02_sensitivity.R done")
