# FUnction to get predictions from MLJAR.
prediction_download <- function(prediction_hid){
  api_url_preddown <- paste("https://mljar.com/api/", API_VERSION, "/download/prediction/" , sep="")
  data <- list( prediction_hid =  prediction_hid)
  resp <- POST(api_url_preddown, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  prediction <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  return(prediction)
}
