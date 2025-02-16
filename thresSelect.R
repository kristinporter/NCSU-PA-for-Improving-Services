#-------------------------#
thresholdsSelect <- selectedThresholds # default is same as selected above, or this can be edited to specify a different set of thresholds
#-------------------------#

equityResults <- calc_equity_metrics_all(
  predProbs = learnersResults$predProbs,
  learnerNames = learnerNames,
  thresholdsSelect = thresholdsSelect,
  equityVars = learnerSpec$equityVars,
  outcomeName = learnerSpec$outcomeName
)

