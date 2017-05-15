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

create_experiment <- function(title, description, metric, validation_scheme, task,
                              parent_project, params){
  #' creates experiment from given parameters
  token <- .get_token()
  api_url_create_experiment <- paste("https://mljar.com/api/", API_VERSION, "/experiments" , sep="")
  #data <- list( title="aa", decription="asasa", metric="logloss", validation_scheme="Split 80/20, Shuffle",
  #task="bin_class", compute_now = 1, parent_project= project_hid, params="")
  data <- list(title = title,
               description = description,
               metric = metric,
               validation_scheme = validation_scheme,
               task = task,
               compute_now = 1,
               parent_project = parent_project,
               params = params)
  resp <- POST(api_url_create_experiment, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    sprintf("Experiment succesfully created!")
  }
}

add_experiment_if_not_exists <- function(validation_dataset, ...){


}
