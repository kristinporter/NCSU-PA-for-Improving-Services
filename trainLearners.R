# running model results all learners in learnerSpec (all specified in 01_Learner_Specification.Rmd)
learnersResults <- train_learners(
  learnerSpec = learnerSpec,
  recipes = recipes,
  mainRecipes = mainRecipes
)

 # this returns cross-validated means and SE for AUC-PR and AUC-ROC
modelResults <- learnersResults$modelResults
 # this is predicted probabilities, from when obs are in validation set
predProbs    <- learnersResults$predProbs 
