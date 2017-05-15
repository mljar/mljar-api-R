get_prediction <- function(project_hid, dataset_hid, result_hid){
  #' Get prediction.
  api_url_prediction <- paste("https://mljar.com/api/", API_VERSION, "/predictions",
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
