#-------------------------#
equityVarSelect    <- "race_SS"
learnerNameSelect  <- "glm_predSet_ms"
outcomeDescription <- c("Predicted as \n NOT dropping out", "Predicted as \n dropping out")
# Insert the title of the graph
graphTitle         <- "Learner fairness from cross-validation"
metricType         <- "fdr" 
#-------------------------#

selectedMetrics <- get_selectedMetrics(metricType)

# subset down to just that learner
learnerPredProbs <- learnersResults$predProbs %>%
  filter(learnerName == learnerNameSelect)

learnerMetricsThresEquity <- conf_metrics_thres_learner(
    predProbs = learnerPredProbs,
    outcomeName = learnerSpec$outcomeName,
    thresSeq = seq(0.0, 1.00, 0.05), # Do not change. Fixed by Notebook.
    equityVar = equityVarSelect 
)

learnerMetricsThresEquityLong <-
  learnerMetricsThresEquity %>%
  dplyr::mutate(thresholdC = as.character(threshold)) %>%
  dplyr::filter(threshold %in% as.character(selectedThresholds)) %>%
  dplyr::select(learnerName, threshold, all_of(equityVarSelect), !!selectedMetrics) %>%
  tidyr::pivot_longer(-c(learnerName, threshold, all_of(equityVarSelect)),
                         names_to = "metricType",
                         values_to = "value" ) %>%
  dplyr::mutate(threshold = as.factor(threshold))

p <- conf_mat_bar_plot(plotData = learnerMetricsThresEquityLong,
                       equityVar = equityVarSelect,
                       metricType = metricType,
                       outcomeDescription = outcomeDescription,
                       graphTitle = graphTitle)

print(p)
