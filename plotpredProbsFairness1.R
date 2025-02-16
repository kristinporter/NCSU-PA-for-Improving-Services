#-------------------------#
equityVarSelect <- "gender_SS" # limit to one
learnerNamesSelect <- c("glm_predSet_bm","glm_predSet_ms") # multiple allowed
#-------------------------#

probsGraphs <- NULL
for(i in 1:length(learnerNamesSelect))
{
  probsGraphs[[i]] <- 
    pred_prob_graph(
      predProbs = learnersResults$predProbs,
      learnerName = learnerNamesSelect[i],
      splitCategory = equityVarSelect
    )
}
ggarrange(plotlist = probsGraphs)
