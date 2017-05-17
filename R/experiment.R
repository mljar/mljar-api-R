get_experiments <- function(project_hid){
  #' Gets list of available experiments for given project
  api_url_experiments <- paste("https://mljar.com/api/", API_VERSION, "/experiments",
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

get_experiment <- function(experiment_hid){
  #' Gets experiment details
  api_url_experiments <- paste("https://mljar.com/api/", API_VERSION, "/experiments/",
                               experiment_hid, sep="")
  rp <- .get_json_from_get_query(api_url_experiments)
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

create_experiment <- function(data){
  #' creates experiment from given parameters
  token <- .get_token()
  api_url_create_experiment <- paste("https://mljar.com/api/", API_VERSION, "/experiments" , sep="")
  resp <- POST(api_url_create_experiment, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    sprintf("Experiment succesfully created!")
  }
  experiment_details <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  return(experiment_details)
}

add_experiment_if_not_exists <- function(project_hid, train_dataset, valid_dataset, experiment_title,
                                         project_task, validation_kfolds, validation_shuffle,
                                         validation_stratify, validation_train_split, algorithms, metric,
                                         tuning_mode, time_constraint, create_ensemble){
  # check validation parameters
  if (!is.null(valid_dataset)){
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

  if (length(train_dataset$dataset$column_usage_min['cols_to_fill_na']) > 0){
    dataset_preproc$na_fill <- "na_fill_median"
  }
  if (length(train_dataset$dataset$column_usage_min['cols_to_convert_categorical']) > 0){
    dataset_preproc$convert_categorical <- "categorical_to_int"
  }
  expt_params <- list(
    train_dataset = list(id = train_dataset$dataset$hid, title = train_dataset$dataset$title),
    algs = algorithms,
    preproc = dataset_preproc,
    single_limit = time_constraint,
    ensemble = create_ensemble,
    random_start_cnt = MLJAR_TUNING_MODES[tuning_mode]["random_start_cnt"],
    hill_climbing_cnt =  MLJAR_TUNING_MODES[tuning_mode]["hill_climbing_cnt"]
  )
  if (!is.null(valid_dataset)){
    expt_params$vald_dataset = list(id = valid_dataset$dataset$hid, title = valid_dataset$dataset$title)
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
  params <- jsonlite::toJSON(expt_params)
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
  create_experiment(project_hid, experiment_data)
}
