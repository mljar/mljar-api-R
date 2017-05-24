#' Gets list of available experiments for given project
#'
#' @param project_hid character with project identifier
#'
#' @return  structure with parsed experiments and http response
#' @export
get_experiments <- function(project_hid){
  api_url_experiments <- paste(MLAR_API_PATH, API_VERSION, "/experiments",
                               "?project_id=", project_hid, sep="")
  rp <- .get_json_from_get_query(api_url_experiments)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      experiments = parsed,
      response = resp
    ),
    class = "get_experiments"
  )
}

print.get_experiments <- function(x, ...) {
  cat("<MLJAR experiments >\n", sep = "")
  str(x$experiments)
  invisible(x)
}

#' Gets experiment details
#'
#' @param experiment_hid character with experiment identifier
#'
#' @return structure with parsed experiment and http response
#' @export
get_experiment <- function(experiment_hid){
  api_url_experiment <- paste(MLAR_API_PATH, API_VERSION, "/experiments/",
                               experiment_hid, sep="")
  rp <- .get_json_from_get_query(api_url_experiment)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      experiment = parsed,
      response = resp
    ),
    class = "get_experiment"
  )
}

print.get_experiment <- function(x, ...) {
  cat("<MLJAR experiment >\n", sep = "")
  str(x$experiment)
  invisible(x)
}

#' Creates experiment from given parameters
#'
#' @param data list of experiment parameters
#'
#' @return experiment details parsed by fromJSON
#' @export
#'
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON
create_experiment <- function(data){
  token <- .get_token()
  api_url_create_experiment <- paste(MLAR_API_PATH, API_VERSION, "/experiments" , sep="")
  resp <- POST(api_url_create_experiment, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    print(sprintf("Experiment succesfully created!"))
  }
  experiment_details <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  return(experiment_details)
}

#' Add experiment if not exists
#'
#' Check if experiment exists, verifies parameters, creates data
#' to create_experiment function and finally starts creation of
#' MLJAR experiment.
#'
#' @param project_hid character with project identifier
#' @param train_dataset character with path to training dataset
#' @param valid_dataset character with path to validation dataset
#' @param experiment_title character with experiment title
#' @param project_task character with project task
#' @param validation_kfolds number of folds to be used in validation
#' @param validation_shuffle boolean which specify if shuffle samples before training
#' @param validation_stratify boolean which decides whether samples will be
#' divided into folds with the same class distribution
#' @param validation_train_split ratio how to split training dataset into train and validation
#' @param algorithms list of algorithms to use
#' @param metric charcater with metric
#' @param tuning_mode tuning mode
#' @param time_constraint numeric with time limit to calculate algorithm
#' @param create_ensemble whether or not to create ensemble
#'
#' @return experiment details structure
#'
#' @export
add_experiment_if_not_exists <- function(project_hid, train_dataset, valid_dataset, experiment_title,
                                         project_task, validation_kfolds, validation_shuffle,
                                         validation_stratify, validation_train_split, algorithms, metric,
                                         tuning_mode, time_constraint, create_ensemble){
  if (!is.null(valid_dataset)){
    # check validation parameters
    validation = "With dataset"
  } else {
    if (!is.null(validation_train_split)){
      percents = round(validation_train_split * 100)
      validation = paste0("Split ", percents , "/", 100-percents)
    } else{
      validation = paste0(validation_kfolds, "-fold CV")
    }
    if (validation_shuffle){
      validation = paste0(validation, ", Shuffle")
    }
    if (validation_stratify && project_task == 'bin_class'){
      validation = paste0(validation, ", Stratify")
    }
    if (validation_stratify && project_task != 'bin_class'){
      warning("Cannot use stratify in validation for your project task.
              Omitting this option in validation.")
    }
  }
  # check metric parameters
  if (is.null(metric) || metric == "" || !(metric %in% names(MLJAR_METRICS))){
    metric = MLJAR_DEFAULT_METRICS[project_task]
  }
  # check tuning mode parameters
  if (is.null(tuning_mode) || tuning_mode == "" || !(tuning_mode %in% names(MLJAR_TUNING_MODES))){
    tuning_mode = MLJAR_DEFAULT_TUNING_MODE
  }
  # check algorithms parameters
  if (is.null(algorithms) || length(algorithms) == 0 || algorithms == ""){
    algorithms = MLJAR_DEFAULT_ALGORITHMS[project_task]
  }
  # set default preprocessing if needed
  dataset_preproc <- list()
  dataset_preproc$na_fill <- "na_fill_median"
  if (length(train_dataset$column_usage_min['cols_to_fill_na']) > 0){
    dataset_preproc$na_fill <- "na_fill_median"
  }
  if (length(train_dataset$column_usage_min['cols_to_convert_categorical']) > 0){
    dataset_preproc$convert_categorical <- "categorical_to_int"
  }
  if (length(dataset_preproc)==0) dataset_preproc={}
  expt_params <- list(
    train_dataset = list(id = train_dataset$hid, title = train_dataset$title),
    algs = algorithms,
    preproc = dataset_preproc,
    single_limit = time_constraint,
    ensemble = create_ensemble,
    random_start_cnt = MLJAR_TUNING_MODES[[tuning_mode]][["random_start_cnt"]],
    hill_climbing_cnt =  MLJAR_TUNING_MODES[[tuning_mode]][["hill_climbing_cnt"]]
  )
  if (!is.null(valid_dataset)){
    expt_params$vald_dataset = list(id = valid_dataset$hid, title = valid_dataset$title)
  }

  # checks whether title of experiment is different
  all_experiments = get_experiments(project_hid)
  if (length(all_experiments$experiments) > 0) {
    for(i in 1:length(all_experiments$experiments)) {
      if (all_experiments$experiments[[i]]$title == experiment_title){
        stop("The experiment with specified title already exists\nPlease rename your new experiment with new parameters setup.")
      }
    }
  }
  params <- jsonlite::toJSON(expt_params, auto_unbox =TRUE)
  #' if everything is fine untill this point we can create data list to
  #' build a new experiment
  experiment_data <- list(title =  experiment_title,
                          description = "",
                          metric = metric,
                          validation_scheme = validation,
                          task = project_task,
                          compute_now = 1,
                          parent_project = project_hid,
                          params = params
                          )
  create_experiment(experiment_data)
}
