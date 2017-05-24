#' Function to get predictions from MLJAR.
#'
#' @param prediction_hid prediction identifier
#'
#' @return data.frame with prediction
#'
#' @importFrom httr POST
#'
#' @export
prediction_download <- function(prediction_hid){
  token <- .get_token()
  api_url_preddown <- paste(MLAR_API_PATH, API_VERSION, "/download/prediction/" , sep="")
  data <- list( prediction_id =  prediction_hid)
  resp <- POST(api_url_preddown, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 200, "Error in prediction download!")
  tmpfilepath <- paste0(tempfile(),".csv")
  file.create(tmpfilepath)
  write(content(resp), file = tmpfilepath)
  prediction <- read.csv(tmpfilepath)
  file.remove(tmpfilepath)
  return(prediction)
}
