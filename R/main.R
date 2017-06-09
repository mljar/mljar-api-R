#' Get results statistics
#'
#' @param results results structure
#'
#' @return list with numbers of jobs: initiated, learning, done, error
.get_results_stats <- function(results){
  resstats <- list()
  resstats$initiated_cnt <- 0
  resstats$learning_cnt  <- 0
  resstats$done_cnt      <- 0
  resstats$error_cnt     <- 0
  for (r in results$results){
    if (r$status == "Initiated"){
      resstats$initiated_cnt = resstats$initiated_cnt + 1
    } else if (r$status == "Learning"){
      resstats$learning_cnt = resstats$learning_cnt + 1
    } else if (r$status == "Done"){
      resstats$done_cnt = resstats$done_cnt + 1
    } else {
      resstats$error_cnt = resstats$error_cnt + 1
    }
  }
  return(resstats)
}

#' Gives info about remaining training time
#'
#' @param exp experiment structure
#' @param res_stats results statistics structure
#'
#' @return numeric with estimated time
.asses_total_training_time <- function(exp, res_stats){
  single_alg_limit <- exp$experiment$params$single_limit
  if (is.null(single_alg_limit)){
    single_alg_limit <- 5
  } else {
    single_alg_limit <- as.numeric(single_alg_limit)
  }
  total <- (res_stats$initiated_cnt * single_alg_limit) / max(c(res_stats$learning_cnt,1))
  total <- total + 0.5 * single_alg_limit
  return(total)
}

#' Get best result
#'
#' Returns best result from given experiment and results stats.
#'
#' @param exp experiment structure
#' @param curr_results currect results structure
#'
#' @return results structure with best results
.get_best_result <- function(exp, curr_results){
  the_best_result <- NULL
  min_value       <- 10e12
  if (exp$experiment$compute_now == 1 || exp$experiment$compute_now == 2) {
    if (!(exp$experiment$metric %in% MLJAR_OPT_MAXIMIZE)){
      opt_direction <- 1
    } else {
      opt_direction <- -1
    }
    for(res in curr_results$results){
      if(is.null(res$metric_value)) next
      if(res$metric_value * opt_direction < min_value){
        min_value <- res$metric_value*opt_direction
        the_best_result <- res
      }
    }
  }
  return(the_best_result)
}

#' Wait till all models trained
#'
#' Waits untill all models are trained and returns best model.
#'
#' @param project_hid character with project identifier
#' @param experiment_hid character with experiment identifier
#'
#' @return best model structure
.wait_till_all_models_trained <- function(project_hid, experiment_hid){
  WAIT_INTERVAL    <- 10.0
  loop_max_counter <- 24*360 # 24 hours of maximum waiting
  results          <- NULL
  while(loop_max_counter > 0){
    loop_max_counter <- loop_max_counter - 1
    rtry <- try({
      curr_results <- get_results(project_hid, experiment_hid)
      exp <- get_experiment(experiment_hid)
      if (exp$experiment$compute_now == 2){
        break
      }
      res_stats <- .get_results_stats(curr_results)
      # printing out info about training process
      eta <- .asses_total_training_time(exp, res_stats)
      if (res_stats$initiated_cnt + res_stats$learning_cnt +
          res_stats$done_cnt + res_stats$error_cnt == 0) {
        eta <- "estimating"
      } else {
        eta = round(eta, 2)
      }
      cat("\r", sprintf(
      "initiated: %s, learning: %s, done: %s, error: %s | ETA: %s minutes               ",
          res_stats$initiated_cnt, res_stats$learning_cnt, res_stats$done_cnt,
          res_stats$error_cnt, eta))
      Sys.sleep(WAIT_INTERVAL)

    }, silent=TRUE)
    if(class(rtry) == "try-error"){
      warning(paste("There were some problems with your model: ", geterrmessage()))
    }
  }
  best_result <- .get_best_result(exp, curr_results)
  return(best_result)
}

#' Starts experiment and returns best model
#'
#' But before verifies if given input data is correct.
#'
#' @param x data.frame/matrix with training data
#' @param y data.frame/matrix with training labels
#' @param validx data.frame/matrix with validation data
#' @param validy data.frame/matrix with validation labels
#' @param proj_title charcater with project title
#' @param exp_title charcater with experiment title
#' @param dataset_title charcater with dataset title
#' @param val_dataset_title charcater with validation dataset title
#' @param metric charcater with metric
#' @param algorithms list of algorithms to use
#' @param validation_kfolds number of folds to be used in validation
#' @param validation_shuffle boolean which specify if shuffle samples before training
#' @param validation_stratify boolean which decides whether samples will be
#' divided into folds with the same class distribution
#' @param validation_train_split ratio how to split training dataset into train and validation
#' @param tuning_mode tuning mode
#' @param create_ensemble whether or not to create ensemble
#' @param single_algorithm_time_limit numeric with time limit to calculate algorithm
#'
#' @return structure with the best model
.start_experiment <- function(x, y, validx, validy, proj_title, exp_title,
                              dataset_title, val_ds_title, metric,
                              algorithms, validation_kfolds, validation_shuffle,
                              validation_stratify, validation_train_split,
                              tuning_mode, create_ensemble, single_algorithm_time_limit){
  task <- .obtain_task(y)
  if (length(algorithms) == 0) {
    algorithms <- ifelse(task == "reg",
                         MLJAR_DEFAULT_ALGORITHMS$regression,
                         MLJAR_DEFAULT_ALGORITHMS$bin_class)
    warning(sprintf("You did not specify algorithms: defaults for task %s are %s",
                    task, paste(algorithms, collapse=" ")))
  }
  if (nchar(metric) == 0) {
    metric <- ifelse(task == "reg",
                     MLJAR_DEFAULT_METRICS$regression,
                     MLJAR_DEFAULT_METRICS$bin_class)
    warning(sprintf("You did not specify metric: defaults for task %s are %s",
                    task, paste(metric, collapse=" ")))
  }
  # create project and datasets
  tmp_data_filename <- .data_to_file(x, y)
  tmp_proj_hid <- .check_if_project_exists(proj_title)
  if (is.null(tmp_proj_hid))
    project_details <- create_project(proj_title, task)
  else {
    print(sprintf("Project <%s> exists.", proj_title))
    project_details <- get_project(tmp_proj_hid)$project
  }
  ds_title <- ifelse(is.null(dataset_title),
                     paste0("Dataset", round(runif(1, 1, 999))),
                     dataset_title )
  dataset <- add_dataset_if_not_exists(project_details$hid,
                                       tmp_data_filename, ds_title)
  if (!is.null(validx) && !is.null(validy)){
    tmp_valid_data_filename <- .data_to_file(validx, validy)
    val_title <- ifelse(is.null(val_ds_title),
                        paste0("Val_dataset", round(runif(1, 1, 999))),
                        val_ds_title)
    valdataset <- add_dataset_if_not_exists(project_details$hid, tmp_valid_data_filename, val_title)
  } else {
    valdataset <- NULL
  }
  # add experiment
  exp_details <- add_experiment_if_not_exists(project_details$hid, dataset$dataset,
                                              valdataset$dataset, exp_title, task,
                                              validation_kfolds, validation_shuffle,
                                              validation_stratify, validation_train_split,
                                              algorithms, metric, tuning_mode,
                                              single_algorithm_time_limit, create_ensemble)
  best_model <- .wait_till_all_models_trained(project_details$hid, exp_details$hid)
  return(best_model)
}

#' MLJAR FIT
#'
#' Verifies parameters and data and tries to run experiment.
#'
#' @param x data.frame/matrix with training data
#' @param y data.frame/matrix with training labels
#' @param validx data.frame/matrix with validation data
#' @param validy data.frame/matrix with validation labels
#' @param proj_title charcater with project title
#' @param exp_title charcater with experiment title
#' @param dataset_title charcater with dataset name
#' @param val_dataset_title charcater with validation dataset name
#' @param metric charcater with metric
#' For binary classification there are metrics:
#' "auc" which is for Area Under ROC Curve,
#' "logloss" which is for Logarithmic Loss.
#' For regression tasks:
#' "rmse" which is Root Mean Square Error,
#' "mse" which is for Mean Square Error,
#' "mase" which is for Mean Absolute Error.
#' @param wait_till_all_done boolean saying whether function should wait
#' till all models are done
#' @param algorithms list of algorithms to use
#' For binary classification task available algorithm are:
#' "xgb" which is for Xgboost,
#' "lgb" which is for LightGBM
#' "mlp" which is for Neural Network,
#' "rfc" which is for Random Forest,
#' "etc" which is for Extra Trees,
#' "rgfc" which is for Regularized Greedy Forest,
#' "knnc" which is for k-Nearest Neighbors,
#' "logreg" which is for Logistic Regression.
#' For regression task there are available algorithms:
#' "xgbr" which is for Xgboost,
#' "lgbr" which is for LightGBM,
#' "rgfr" which is for Regularized Greedy Forest,
#' "rfr" which is for Random Forest,
#' "etr" which is for Extra Trees.
#' @param validation_kfolds number of folds to be used in validation
#' @param validation_shuffle boolean which specify if shuffle samples before training
#' @param validation_stratify boolean which decides whether samples will be
#' divided into folds with the same class distribution
#' @param validation_train_split ratio how to split training dataset into train and validation
#' @param tuning_mode tuning mode
#' @param create_ensemble whether or not to create ensemble
#' @param single_algorithm_time_limit numeric with time limit to calculate algorithm
#'
#' @return structure with the best model
#' @export
mljar_fit <- function(x, y, validx=NULL, validy=NULL,
                      proj_title=NULL, exp_title=NULL,
                      dataset_title=NULL, val_dataset_title=NULL,
                      algorithms = c(), metric = "",
                      wait_till_all_done = TRUE,
                      validation_kfolds = MLJAR_DEFAULT_FOLDS,
                      validation_shuffle = MLJAR_DEFAULT_SHUFFLE,
                      validation_stratify = MLJAR_DEFAULT_STRATIFY,
                      validation_train_split = MLJAR_DEFAULT_TRAIN_SPLIT,
                      tuning_mode = MLJAR_DEFAULT_TUNING_MODE,
                      create_ensemble  = MLJAR_DEFAULT_ENSEMBLE,
                      single_algorithm_time_limit = MLJAR_DEFAULT_TIME_CONSTRAINT){
  if (is.null(proj_title)){
    proj_title <- paste0("Project", round(runif(1, 1, 999)))
  }
  if (is.null(exp_title)){
    proj_title <- paste0("Experiment", round(runif(1, 1, 999)))
  }
  model <- .start_experiment(x, y, validx, validy, proj_title, exp_title,
                             dataset_title, val_dataset_title, metric,
                             algorithms, validation_kfolds, validation_shuffle,
                             validation_stratify, validation_train_split,
                             tuning_mode, create_ensemble,
                             single_algorithm_time_limit)
  return(model)
}

#' MLJAR PREDICT
#'
#' Makes prediction basing on trained model.
#'
#' @param model model or MLJAR result structure
#' @param x_pred data.frame/matrix data to predict
#' @param project_title character with project title
#'
#' @return data.frame with preditction
#' @export
mljar_predict <- function(model, x_pred, project_title){
  if (is.null(model)) {
    stop("Model cannot be null.")
  }
  # checking if prediction data is ok
  x_pred <- as.data.frame(x_pred)
  if (is.null(x_pred)) {
    stop("NULL data")
  }
  # look for project
  proj_hid <- .check_if_project_exists(project_title)
  if (is.null(proj_hid)) stop("Project not found! Check title and try again.")
  # adding prediction dataset
  tmp_data_filename <- .data_to_file(x_pred)
  dspred_title <- paste0("Pred_dataset", round(runif(1, 1, 999)))
  pred_ds  <- add_dataset_if_not_exists(proj_hid, tmp_data_filename, dspred_title, TRUE)
  total_checks <- 1000
  for (i in 1:total_checks){
    prediction <- get_prediction(proj_hid, pred_ds$dataset$hid, model$hid)
    print(sprintf("Downloading prediction - %s", i))
    # for first iteration we send dataset for prediction
    if (i == 1 && length(prediction$prediction) == 0) {
      submit_predict_job(proj_hid, pred_ds$dataset$hid, model$hid)
    }
    if (length(prediction$prediction) > 0) {
      pred <- prediction_download(prediction$prediction[[1]]$hid)
      delete_dataset(pred_ds$dataset$hid)
      return(pred)
    }
    Sys.sleep(10)
  }
  return(NULL)
}

#' Gives data.frame with basic data of all models
#'
#' You can later get some specific model by calling
#' e.g. `get_result(model_hid)'.
#'
#' @param project_title character with project title
#' @param exp_title character with experiment title
#'
#' @return data.frame with model's "hid", "model_type", "metric_value",
#' "metric_type"
#'
#' @export
get_all_models <- function(project_title, exp_title) {
  # Look for project title
  flag.proj.title <- FALSE
  prj_hid <- .check_if_project_exists(project_title)
  if (is.null(prj_hid))
    stop("MLJAR cannot find a project with such a title. Check and try again.")
  # Look for experiment title
  flag.proj.exp <- FALSE
  ge <- get_experiments(prj_hid)
  if (length(ge$experiments) == 0) stop("No experiments found.")
  for(i in 1:length(ge$experiments)) {
    if (ge$experiments[[i]]$title == exp_title){
      flag.proj.exp <- TRUE
      break
    }
  }
  if (flag.proj.exp == FALSE)
    stop("MLJAR cannot find an experiment with such a title. Check and try again.")
  exp_hid <- ge$experiments[[i]]$hid
  exp <- get_experiment(exp_hid)
  if (exp$experiment$compute_now != 2)
    stop("Experiment still in progress. Wait till its done!")
  curr_results <- get_results(prj_hid, exp_hid)
  column.names <- c("hid", "model_type", "metric_value",
                    "metric_type", "validation_scheme")
  tmp_sa <- sapply(curr_results$results,
             function(x) c(x$hid, x$model_type, x$metric_value,
                           x$metric_type, x$validation_scheme),
             simplify = FALSE, USE.NAMES = TRUE)
  df_res <- t(as.data.frame(tmp_sa,
                            row.names = column.names,
                            col.names = 1:length(tmp_sa)))
  df_res <- data.frame(df_res, row.names = NULL)
  return(df_res)
}

