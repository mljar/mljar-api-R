#' Get results statistics
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

#' checks if data is in good format
.data_check <- function(x, y){
  if(length(dim(y))>1 && dim(x)[2]>1){
    stop("Sorry, multiple outputs are not supported in MLJAR")
  }
  if(dim(y)[1]!=dim(x)[1]){
    stop("Sorry, there is a missmatch between X and y matrices shapes")
  }
}

#' converts data to temporary file
.data_to_file <- function(x, y){
  # first we check if data is valid
  .data_check(x, y)
  # now it's time to convert to data frame
  dataxy <- as.data.frame(x)
  dataxy["target"] <- y
  tmpfilepath <- tempfile()
  file.create(tmpfilepath)
  write.csv(dataxy, file = tmpfilepath, row.names = F)
  return(tmpfilepath)
}

# determines what kind of task is that basing on y
.obtain_task <- function(y){
  return(length(unique(y))>2, "reg", "bin_class")
}

# waits untill all models are trained
.wait_till_all_models_trained() <- function(){
  WAIT_INTERVAL    <- 10.0
  loop_max_counter <- 24*360 # 24 hours of maximum waiting
  results          <- NULL
  #TODO
}

# starts experiment
.start_experiment <- function(x, y, validx, validy, proj_title, exp_title, metric,
                              algorithms, validation_kfolds, validation_shuffle,
                              validation_stratify, validation_train_split,
                              tuning_mode, create_ensemble, single_algorithm_time_limit){
  task <- .obtain_task(y)
  # create project and datasets
  project_details <- create_project(proj_title, task)
  tmp_data_filename <- .data_to_file(x, y)
  ds_title <- paste0("Dataset", round(runif(1, 1, 999)))
  ds_details <- add_dataset_if_not_exists(project_details$hid, tmp_data_filename, ds_title)
  # TODO Validation data ???
  if (!is.null(validx) && !is.null(validy)){
    tmp_valid_data_filename <- .data_to_file(validx, validy)
    val_title <- paste0("Val_dataset", round(runif(1, 1, 999)))
    val_details <- add_dataset_if_not_exists(project_details$hid, tmp_valid_data_filename, val_title)
  }
  # add experiment
  exp_details <- add_experiment_if_not_exists(project_details$hid, ds_details,
                                              val_details, wait_till_all_done, exp_title,
                                              task, validation_kfolds, validation_shuffle,
                                              validation_stratify, validation_train_split,
                                              algorithms, metric, tuning_mode,
                                              single_algorithm_time_limit, create_ensemble)
  .wait_till_all_models_trained()
  #TODO
}

mljar_fit <- function(x, y, validx=NULL, validy=NULL,
                      proj_title=NULL, exp_title=NULL,
                      algorithms = c(), metric = '',
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
  if (length(algorithms) == 0){
    stop("You must specify non-empty vector of algorithms to use.")
  }

}

mljar_predict <- function(){

}
