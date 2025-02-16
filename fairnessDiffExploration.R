#-------------------------#
learnersSelect   <- "glm_predSet_ms"
equityVarsSelect <- "race_SS_i"
thresholdsSelect <- selectedThresholds
#-------------------------#

equityResultsSubset <- equityResults %>%
  filter(learnerName %in% learnersSelect) %>%
  filter(equityVar %in% equityVarsSelect) %>%
  filter(threshold %in% thresholdsSelect)

equityResultsSubsetRound <- round_table(equityResultsSubset) %>%
    rename(learner = learnerName, var = equityVar, cat = category, thres = threshold)

print(equityResultsSubsetRound)
