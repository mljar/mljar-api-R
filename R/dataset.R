get_datasets <- function(project_hid) {
  #' Gets list of available datasets
  api_url_datasets <- paste("https://mljar.com/api/", API_VERSION, "/datasets?project_id=", project_hid, sep="")
  rp <- .get_json_from_get_query(api_url_datasets)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      datasets = parsed,
      response = resp
    ),
    class = "get_datasets"
  )
}

print.get_datasets <- function(x, ...) {
  cat("<MLJAR datasets >\n", sep = "")
  str(x$datasets)
  invisible(x)
}

get_dataset <- function(dataset_hid) {
  #' Get data from a dataset of specified hid
  api_url_dataset_hid <- paste("https://mljar.com/api/", API_VERSION, "/datasets/", dataset_hid, sep="")
  rp <- .get_json_from_get_query(api_url_dataset_hid)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      dataset = parsed,
      response = resp
    ),
    class = "get_dataset"
  )
}

print.get_dataset <- function(x, ...) {
  cat("<MLJAR dataset >\n", sep = "")
  str(x$dataset)
  invisible(x)
}

delete_dataset <-function(dataset_hid){
  #' deletes project
  token <- .get_token()
  api_url_dataset_hid <- paste("https://mljar.com/api/", API_VERSION, "/datasets/", hid, sep="")
  resp <- DELETE(api_url_dataset_hid, add_headers(Authorization = paste("Token", token)))
  if (status_code(resp)==204 || status_code(resp)==200){
    sprintf("Project <%s> succesfully deleted!", hid)
  }
}

add_new_dataset <- function(project_hid, filename, title){
  # TODO in future create temporary csv file from data.frame
  dst_path <- upload_file(project_hid, filename)

  # TODO check if prediction only
  prediction_only <- 1

  token <- .get_token()
  api_url_new_dataset <- paste("https://mljar.com/api/", API_VERSION, "/datasets" , sep="")
  data <- list(
    title = title,
    file_path = dst_path,
    file_name = filename,
    file_size = round(file.info(filename)$size/1024, 2),
    derived = 0,
    valid = 0,
    parent_project = project_hid,
    meta = '',
    data_type = 'tabular',
    scope = 'private',
    prediction_only = prediction_only
  )
  resp <- POST(api_url_new_dataset, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    sprintf("Dataset created!")
  }

}


#' Wait till all datasets are valid
#'
#'  Waits till all datasets is valid. If all valid it returns no error,
#' if wait time is exceeded and there is any dataset not valid then
#' it returns TRUE.
#'
#' @param project_hid hid of the project
#'
#' @return TRUE if all datasets are valid
#'
.wait_till_all_datasets_are_valid <-function(project_hid){
  total_checks  <- 120
  time_interval <- 5 # sleep for 5 sec every iteration
  for (i in 1:total_checks){
    datasets_list <- get_datasets(project_hid)
    if (is.null(datasets_list)){
      sprintf("No datasets")
      return(TRUE)
    } else {
      tmpcnt = 0
      for (k in 1:length(dl$datasets)){
        tmpcnt = tmpcnt + dl$datasets[[k]]$valid
      }
      if (tmpcnt == length(dl$datasets)){
        sprintf("All datasets are valid")
        return(TRUE)
      }
    }
    Sys.sleep(time_interval)
  }
  stop("Some datasets are invalid.")
}
