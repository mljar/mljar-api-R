get_datasets <- function() {
  #' Gets list of available datasets
  api_url_datasets <- paste("https://mljar.com/api/", API_VERSION, "/datasets" , sep="")
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

get_dataset <- function(hid) {
  #' Get data from a dataset of specified hid
  api_url_dataset_hid <- paste("https://mljar.com/api/", API_VERSION, "/projects/", hid, sep="")
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

