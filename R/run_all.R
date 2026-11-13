# run_all.R — run the full pipeline from the raw exports.
#   DATA_DIR=/path/to/raw Rscript R/run_all.R
# Fitted models are cached in data/fits/ (git-ignored); delete a file there to refit that model.
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
for (s in c("R/00_clean_data.R", "R/04_descriptives.R", "R/01_heterogeneity.R", "R/02_sensitivity.R", "R/03_moderators.R")) {
  message("== ", s); source(s, local = new.env())
}
