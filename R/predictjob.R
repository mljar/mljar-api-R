#' Submits dataset for MLJAR prediction
#'
#' @param project_hid character with project identifier
#' @param dataset_hid character with dataset identifier
#' @param result_hid character with result identifier
#'
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @export
submit_predict_job <- function(project_hid, dataset_hid, result_hid){
  token <- .get_token()
  data <- list(predict_params = jsonlite::toJSON(list(project_id =  project_hid,
                                            project_hardware = 'cloud',
                                            algorithms_ids = list(result_hid),
                                            dataset_id = dataset_hid,
                                            cv_models = 1),
                                       auto_unbox =TRUE)
              )
  query <- paste(MLAR_API_PATH, API_VERSION, "/predict/" , sep="")
  resp <- POST(query, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 200, "Predict MLJAR job failed")
}
