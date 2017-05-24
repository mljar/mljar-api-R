#' Gets MLJAR predictions
#'
#' @param project_hid character with project identifier
#' @param dataset_hid character with dataset identifier
#' @param result_hid character with result identifier
#'
#' @return structure with parsed prediction and http response
#' @export
get_prediction <- function(project_hid, dataset_hid, result_hid){
  api_url_prediction <- paste(MLAR_API_PATH, API_VERSION, "/predictions",
                               "?project_id=", project_hid, "&dataset_id=",
                               dataset_hid, "&result_id=", result_hid, sep="")
  rp <- .get_json_from_get_query(api_url_prediction)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      prediction = parsed,
      response = resp
    ),
    class = "get_prediction"
  )
}

print.get_prediction <- function(x, ...) {
  cat("<MLJAR prediction >\n", sep = "")
  str(x$prediction)
  invisible(x)
}
