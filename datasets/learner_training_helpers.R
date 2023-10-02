get_model_spec <- function(ML)
{

    # for generalized linear model
  if (ML == "glm"){

    # setting glm Model Specifications
    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glm") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = 0)

  } else if (ML == "lasso"){

    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = tune(),
                        mixture = 1)

  } else if (ML == "random_forest"){

    model_spec <-
      parsnip::rand_forest() %>%
      parsnip::set_engine("ranger") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(mtry = tune(),         # the tuning function is a placeholder to be applied to tune_grid later on
                        trees = tune(),
                        min_n = tune(),
                        importance = "permutation")

  } else if (ML == "naive_bayes"){

    model_spec <-
      parsnip::naive_Bayes() %>%
      parsnip::set_engine("naivebayes") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(smoothness = tune(),
                        Laplace = tune())

  } else if (ML == "xgboost"){

    model_spec <-
      parsnip::boost_tree() %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(tree_depth = tune(),
                        trees = tune(),
                        learn_rate = tune())

  } else if (ML == "svm_poly"){

    model_spec <-
      parsnip::svm_poly() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tune(),
                        degree = tune(),
                        scale_factor = tune(),
                        margin = tune())

  } else if (ML == "svm_rbf"){

    model_spec <-
      parsnip::svm_rbf() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tune(),
                        rbf_sigma = tune(),
                        margin = tune())

  } else if (ML == "neural_network"){

    model_spec <-
      parsnip::mlp() %>%
      parsnip::set_engine("nnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(hidden_units = tune(),
                        penalty = tune(),
                        epochs = tune())

  } else
  {
    print(paste0("Invalid ML:", ML))
  }

  return(model_spec)
}

pick_threshold <- function(
    learnerMetricsThres, learnerNameSelect,
    metricName, metricNumber, below
)
{
  learnerRows <- learnerMetricsThres %>%
    dplyr::filter(learnerName == learnerNameSelect)

  if(below)
  {
    candidates <- learnerRows[learnerRows[,metricName] < metricNumber,]
    thres <- candidates$threshold[which.max(candidates[,metricName])]
  } else
  {
    candidates <- learnerRows[learnerRows[,metricName] > metricNumber,]
    thres <- candidates$threshold[which.min(candidates[,metricName])]
  }

  return(thres)
}

# filter out learner results to just a subset of learners
filter_results <- function(learnersResults, learnerNamesFocus)
{
  # first make a copy
  learnersResultsFocus <- learnersResults

  # filter out to just relevant learners from data frames
  learnersResultsFocus$modelResults <- learnersResultsFocus$modelResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$predProbs <- learnersResultsFocus$predProbs %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$rocResults <- learnersResultsFocus$rocResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$prResults <- learnersResultsFocus$prResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$learnersMetrics <- learnersResultsFocus$learnersMetrics %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)

  # list of modelFits
  # ugly code :(
  learnersResultsFocus$modelFits <- list()
  learnersResultsFocus$finalWorkflows <- list()
  for(l in learnerNamesFocus)
  {
    learnersResultsFocus$modelFits[[l]] <- learnersResults$modelFits[[
      which(names(learnersResults$modelFits) == l)
    ]]
    learnersResultsFocus$finalWorkflows[[l]] <- learnersResults$finalWorkflows[[
      which(names(learnersResults$finalWorkflows) == l)
    ]]
  }

  # add in additional info
  learnersResultsFocus$learnerSpec <- learnerSpec
  learnersResultsFocus$learnerNames <- learnerNamesFocus

  return(learnersResultsFocus)
}

# convert metrics from long to wide
convert_metrics_wide <- function(modelResults)
{
  learnersMetricsMean <- tidyr::pivot_wider(
    modelResults,
    id_cols = c("learnerName", "predSet", "ML"),
    names_from = ".metric",
    values_from = "mean",
    names_prefix = "mean_"
  )

  learnersMetricsStdError <- tidyr::pivot_wider(
      modelResults,
      id_cols = c("learnerName", "predSet", "ML"),
      names_from = ".metric",
      values_from = "std_err",
      names_prefix = "se_"
    ) %>%
    dplyr::select(c("se_accuracy", "se_pr_auc", "se_roc_auc"))

  learnersMetricsWide <- dplyr::bind_cols(
    learnersMetricsMean,
    learnersMetricsStdError
  )

  return(learnersMetricsWide)
}

# create a version of a data.frame with
# rounded digits for pretty printing
round_table <- function(table)
{
  roundTable <- table %>%
    dplyr::mutate_if(is.numeric, round, digits = 5)

  return(roundTable)
}

#' Helper function to split the incoming learners
#' into machine learning, predictor set
#'
#' @param selectedLearners enter selected Learners saved from the previous 02_01_Learner_Training Rmd

split_learner_names <- function(selectedLearners)
{
  # setting up null vectors to store the split ml,
  # predSet and tuningSet

  ML <- NULL
  predSet <- NULL

  for (i in 1:length(selectedLearners))
  {
    learnerSplit <- unlist(stringr::str_split(selectedLearners[i], "_predSet_"))

    ML[i] <- learnerSplit[1]
    predSet[i] <- paste0("predSet_", learnerSplit[2])
  }

  return(list(ML = ML,
              predSet = predSet))
} # learnerNameSplits


