get_results <- function(project_hid, experiment_hid){
  #' Get results - list all models.
  token <- .get_token()
  api_url_results <- paste("https://mljar.com/api/", API_VERSION, "/results/" , sep="")
  datares <- list( project_id =  project_hid,
                   experiment_id =  experiment_hid)
  resp <- POST(api_url_results, add_headers(Authorization = paste("Token", token)),
               body = datares, encode = "form")
  .check_response_status(resp, 200)
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  structure(
    list(
      results = parsed,
      response = resp
    ),
    class = "get_results"
  )
}

print.get_results <- function(x, ...) {
  cat("<MLJAR results >\n", sep = "")
  str(x$results)
  invisible(x)
}
