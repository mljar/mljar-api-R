#' Gets list of available datasets
#'
#' @param project_hid character with project identifier
#'
#' @return structure with parsed datasets and response
#' @export
get_datasets <- function(project_hid) {
  #' Gets list of available datasets
  api_url_datasets <- paste(MLAR_API_PATH, API_VERSION, "/datasets?project_id=", project_hid, sep = "")
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

#' Gets dataset
#'
#' @param dataset_hid character with dataset identifier
#'
#' @return structure with parsed dataset and response
#' @export
get_dataset <- function(dataset_hid) {
  api_url_dataset_hid <- paste(MLAR_API_PATH, API_VERSION, "/datasets/", dataset_hid, sep="")
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

#' Deletes dataset
#'
#' @param dataset_hid character with dataset identifier
#' @export
delete_dataset <-function(dataset_hid){
  token <- .get_token()
  api_url_dataset_hid <- paste(MLAR_API_PATH, API_VERSION, "/datasets/", dataset_hid, sep="")
  resp <- DELETE(api_url_dataset_hid, add_headers(Authorization = paste("Token", token)))
  if (status_code(resp)==204 || status_code(resp)==200){
    sprintf("Dataset <%s> succesfully deleted!", dataset_hid)
  }
}

#' Adds new dataset
#'
#' @param project_hid character with project identifier
#' @param filename character with filename containing data
#' @param title title of dataset
#' @param prediction_only boolean determining if data is used only for prediction
#'
#' @return parsed by toJSON dataset details
#' @export
#'
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
add_new_dataset <- function(project_hid, filename, title, prediction_only=FALSE){
  dst_path <- upload_file(project_hid, filename)

  prediction_only <- as.integer(prediction_only)

  token <- .get_token()
  api_url_new_dataset <- paste(MLAR_API_PATH, API_VERSION, "/datasets" , sep="")
  data <- list(
    title = title,
    file_path = dst_path,
    file_name = filename,
    file_size = round(file.info(filename)$size/1024, 2),
    derived = 0,
    valid = 0,
    parent_project = project_hid,
    meta = '',
    data_type = "tabular",
    scope = "private",
    prediction_only = prediction_only
  )
  resp <- POST(api_url_new_dataset, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    print(sprintf("Dataset <%s> created!", title))
  }
  dataset_details <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),
                                        simplifyVector = FALSE)
  return(dataset_details)
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
    if (length(datasets_list$datasets) == 0){
      sprintf("No datasets")
      return(TRUE)
    } else {
      tmpcnt = 0
      for (k in 1:length(datasets_list$datasets)){
        tmpcnt = tmpcnt + datasets_list$datasets[[k]]$valid
      }
      if (tmpcnt == length(datasets_list$datasets)){
        sprintf("All datasets are valid")
        return(TRUE)
      }
    }
    Sys.sleep(time_interval)
  }
  stop("Some datasets are invalid.")
}

#' Verify if columns have correct structure
#'
#' At least one column must be Target and this is verified on server site.
#'
#' @param dataset_hid dataset hid code
#'
#' @return TRUE if correct, FALSE if not
#'
.accept_dataset_column_usage <- function(dataset_hid){
  token <- .get_token()
  api_url_new_dataset <- paste(MLAR_API_PATH, API_VERSION, "/accept_column_usage/" , sep="")
  data <- list(dataset_id = dataset_hid)
  resp <- POST(api_url_new_dataset, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  return(ifelse(status_code(resp)==200, TRUE, FALSE))
}

#' Add dataset if not exists
#'
#' Checks parameters before adding new dataset and verifies
#' if it doesn't exists already.
#'
#' @param project_hid character with project identifier
#' @param filename character with filename containing data
#' @param title title of dataset
#' @param prediction_only boolean determining if data is used only for prediction
#'
#' @return parsed dataset details
#' @export
add_dataset_if_not_exists <- function(project_hid, filename, title, prediction_only=FALSE){
  .wait_till_all_datasets_are_valid(project_hid)
  ds <- get_datasets(project_hid)
  if (length(ds$datasets)>0) {
    for(i in 1:length(ds$datasets)) {
      if (ds$datasets[[i]]$title == title) {
        stop("Dataset with the same name already exists")
      }
    }
  }
  dataset_details <- add_new_dataset(project_hid, filename, title, prediction_only)
  .wait_till_all_datasets_are_valid(project_hid)
  if (!.accept_dataset_column_usage(dataset_details$hid)){
    stop("There was a problem with accept column usage for your dataset.")
  }
  new_dataset <- get_dataset(dataset_details$hid)
  if (!new_dataset$dataset$valid){
    stop("Sorry, your dataset cannot be read by MLJAR.\nPlease report this to us - we will fix it")
  }
  if (is.null(new_dataset$dataset$column_usage_min)){
    stop("Something bad happend! There is no attributes usage defined for your dataset")
  }
  return(new_dataset)
}
