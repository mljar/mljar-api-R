submit_predict_job <- function(project_hid, dataset_hid, result_hid){

  data <- list(predict_params = toJSON(list(project_id =  project_hid,
                                            project_hardware = 'cloud',
                                            algorithms_ids = list(result_hid),
                                            dataset_id = dataset_hid,
                                            cv_models = 1),
                                       auto_unbox =TRUE)
              )
  query <- paste("https://mljar.com/api/", API_VERSION, "/predict/" , sep="")

  rp <- .get_json_from_post_query(query, data)
  resp <- rp$resp
  parsed <- rp$parsed
  return(parsed)
    .check_response_status(resp, 200, "Predict MLJAR job failed")
}
