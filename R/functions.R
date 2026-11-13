# functions.R — shared settings and helpers for the model scripts
suppressPackageStartupMessages({ library(brms); library(dplyr); library(tidyr); library(loo); library(readr) })
options(mc.cores = max(1, min(4, parallel::detectCores())), brms.backend = Sys.getenv("BRMS_BACKEND", "rstan"))
if (nzchar(Sys.getenv("RSTAN_BOOST_LIB"))) rstan::rstan_options(boost_lib = Sys.getenv("RSTAN_BOOST_LIB"))

SEED <- 2026
FIT_DIR <- "data/fits"; dir.create(FIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

daily  <- readRDS("data/derived/daily.rds")
person <- readRDS("data/derived/person_level.rds")

OUTCOMES <- c("Positive affect" = "PA", "High-arousal PA" = "PA_high", "Low-arousal PA" = "PA_low",
              "Negative affect" = "NAff", "Irritability" = "Irrit", "Fatigue" = "Fatig")
BENEFIT_POSITIVE <- c("PA", "PA_high", "PA_low")          # higher = better
PAIR <- list(PA_high = c("PA_high", "PA_low"), PA_low = c("PA_high", "PA_low"),
             Irrit = c("Irrit", "Fatig"), Fatig = c("Irrit", "Fatig"))

PRIOR_SLOPE <- c(prior(normal(0, 5), class = "Intercept"), prior(normal(0, 2), class = "b"),
                 prior(student_t(3, 0, 2), class = "sd"), prior(student_t(3, 0, 2), class = "sigma"),
                 prior(lkj(2), class = "cor"))
PRIOR_INT <- PRIOR_SLOPE[1:4, ]

# Analytic days for one outcome: TST and the outcome observed. For a subscale, both subscales of its
# pair must be observed, so the two subscales of a pair are modeled on the same days.
outcome_data <- function(o) {
  need <- if (o %in% names(PAIR)) PAIR[[o]] else o
  daily %>% filter(!is.na(TST_within), !is.na(TST_between), if_all(all_of(need), ~ !is.na(.x))) %>%
    mutate(y = .data[[o]])
}

# iterations: defaults from the manuscript; composite and low-arousal PA are run twice as long to meet R-hat <= 1.01
iters <- function(o, kind = c("het", "mod")) {
  kind <- match.arg(kind)
  base <- if (kind == "het") c(iter = 3000, warmup = 1000) else c(iter = 4000, warmup = 1500)
  if (o %in% c("PA", "PA_low")) base <- base * 2
  base
}

# Fit (or load a cached fit). If any R-hat exceeds 1.01, refit with twice the iterations and warmup,
# up to two times; the longer fits are cached as <name>_x2.rds and <name>_x4.rds.
fit_or_load <- function(name, iter, warmup, ...) {
  for (k in 0:2) {
    mult <- 2^k
    f <- file.path(FIT_DIR, paste0(name, if (k > 0) paste0("_x", mult) else "", ".rds"))
    fit <- brm(..., iter = iter * mult, warmup = warmup * mult, file = f, file_refit = "on_change",
               seed = SEED, refresh = 0, control = list(adapt_delta = 0.95))
    if (max_rhat(fit) <= 1.01) break
    message(name, ": max R-hat ", round(max_rhat(fit), 3), " at ", iter * mult, " iterations; refitting longer")
  }
  fit
}
fit_iter <- function(fit) fit$fit@sim$iter
fit_warmup <- function(fit) fit$fit@sim$warmup

q3 <- function(x) c(mean(x), unname(quantile(x, c(.025, .975))))
fmt <- function(x, d = 2) { s <- formatC(x, format = "f", digits = d); sub("^-", "−", s) }
ci  <- function(v, d = 2) sprintf("%s [%s, %s]", fmt(v[1], d), fmt(v[2], d), fmt(v[3], d))
max_rhat <- function(fit) max(rhat(fit), na.rm = TRUE)
n_div <- function(fit) { np <- nuts_params(fit); sum(np$Value[np$Parameter == "divergent__"]) }
