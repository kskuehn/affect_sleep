# 05_figures.R — Figure 3: credible cross-level interactions (run after 03_moderators.R).
# Each panel shows the model-implied within-person slope of next-day affect on total sleep time (per hour)
# across the moderator on its raw scale, with its 95% credible interval. Lightly shaded regions mark
# moderator values at which that interval excludes zero; tick marks show observed values.
source("R/functions.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

an <- person %>% filter(analytic)
PANELS <- list(
  list(fit = "mod_PA_low_x_ACEs", raw = "ACEs", title = "A. Adverse childhood experiences × low-arousal PA",
       xlab = "Adverse childhood experiences (count)", ylab = "low-arousal PA", step = 0.05, jit = 0.08),
  list(fit = "mod_Irrit_x_CTQ", raw = "CTQ", title = "B. Childhood maltreatment × irritability",
       xlab = "Childhood Trauma Questionnaire total", ylab = "irritability", step = 0.25, jit = 0.3),
  list(fit = "mod_NAff_x_CTQ", raw = "CTQ", title = "C. Childhood maltreatment × negative affect",
       xlab = "Childhood Trauma Questionnaire total", ylab = "negative affect", step = 0.25, jit = 0.3))

# use the longest cached fit for each model (fit_or_load refits at 2x/4x iterations when R-hat > 1.01)
load_fit <- function(name) {
  f <- rev(Filter(file.exists, file.path(FIT_DIR, paste0(name, c("", "_x2", "_x4"), ".rds"))))[1]
  readRDS(f)
}

# runs of x where the interval excludes zero, as rectangles
sig_regions <- function(g) {
  s <- g$lo > 0 | g$hi < 0
  if (!any(s)) return(data.frame(xmin = numeric(0), xmax = numeric(0)))
  r <- rle(s); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
  data.frame(xmin = g$x[starts[r$values]], xmax = g$x[ends[r$values]])
}

make_panel <- function(p) {
  dr <- as_draws_df(load_fit(p$fit))
  m <- mean(an[[p$raw]], na.rm = TRUE); s <- sd(an[[p$raw]], na.rm = TRUE)
  obs <- an[[p$raw]][!is.na(an[[p$raw]])]
  x <- seq(min(obs), max(obs), by = p$step)
  sl <- sapply((x - m) / s, function(z) dr$b_TST_within + dr$`b_TST_within:M` * z)
  g <- data.frame(x = x, est = colMeans(sl), lo = apply(sl, 2, quantile, .025), hi = apply(sl, 2, quantile, .975))
  ybot <- min(g$lo); ytop <- max(g$hi); pad <- 0.06 * (ytop - ybot)
  ggplot(g, aes(x)) +
    geom_rect(data = sig_regions(g), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey92") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey72", alpha = 0.7) +
    geom_line(aes(y = est), linewidth = 0.9) +
    geom_rug(data = data.frame(x = obs + p$jit * (rank(seq_along(obs)) %% 5 - 2) / 2), aes(x = x), sides = "b",
             length = unit(0.04, "npc"), alpha = 0.6) +
    { if (p$raw == "ACEs") scale_x_continuous(breaks = 0:9) } +
    scale_y_continuous(limits = c(ybot - pad * 1.5, ytop + pad)) +
    labs(title = p$title, x = p$xlab, y = paste0("Within-person association of sleep\nwith next-day ", p$ylab, " (per hour)")) +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(size = 10, face = "bold"), axis.title.y = element_text(size = 9))
}

fig3 <- wrap_plots(lapply(PANELS, make_panel), ncol = 1)
dir.create("output/figures", showWarnings = FALSE)
ggsave("output/figures/figure3_moderators.png", fig3, width = 6.5, height = 7.5, dpi = 300, device = ragg::agg_png, bg = "white")
message("05_figures.R done")
