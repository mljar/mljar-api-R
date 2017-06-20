#' Gets results of MLJAR training
#'
#' @param project_hid character with project identifier
#' @param experiment_hid character with experiment identifier
#'
#' @return structure with parsed results and http response
#'
#' @importFrom httr POST
#'
#' @export
get_results <- function(project_hid, experiment_hid){
  token <- .get_token()
  api_url_results <- paste(MLAR_API_PATH, API_VERSION, "/results/" , sep="")
  datares <- list( project_id =  project_hid,
                   experiment_id =  experiment_hid)
  resp <- POST(api_url_results, add_headers(Authorization = paste("Token", token)),
               body = datares, encode = "form")
  .check_response_status(resp, 200)
  parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
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


#' Get model
#'
#' Gets model only if experiment finished and project with such
#' a title and having such an experiment exists.
#'
#' @param project_title character with project title
#' @param exp_title character with experiment title
#' @param model_hid character with experiment identifier
#'
#' @return structure with model parameters
#' @export
get_model <- function(project_title, exp_title, model_hid) {
  # Look for project title
  flag.proj.title <- FALSE
  prj_hid <- .check_if_project_exists(project_title)
  if (is.null(prj_hid))
    stop("MLJAR cannot find a project with such a title. Check and try again.")
  # Look for experiment title
  flag.proj.exp <- FALSE
  ge <- get_experiments(prj_hid)
  if (length(ge$experiments) == 0) stop("No experiments found.")
  for(i in 1:length(ge$experiments)) {
    if (ge$experiments[[i]]$title == exp_title){
      flag.proj.exp <- TRUE
      break
    }
  }
  if (flag.proj.exp == FALSE)
    stop("MLJAR cannot find an experiment with such a title. Check and try again.")
  exp_hid <- ge$experiments[[i]]$hid
  exp <- get_experiment(exp_hid)
  if (exp$experiment$compute_now != 2)
    stop("Experiment still in progress. Wait till its done!")
  flag.mod <- FALSE
  curr_results <- get_results(prj_hid, exp_hid)
  for(res in curr_results$results) {
    if (res$hid == model_hid){
      flag.mod <- TRUE
      break
    }
  }
  if (flag.mod == FALSE)
    stop("MLJAR cannot find an experiment with such a title. Check and try again.")
  return(res)
}
