upload_file <- function(projecthid, filepath){
  
}

.get_signed_url <- function(projecthid, filepath){
  api_version <- "v1"
  api_url_signed_url <- paste("https://mljar.com/api/", api_version, "/s3policy/" , sep="")
  fname = tail(strsplit(filepath, "/")[[1]], n=1)
  data <- list(project_hid = projecthid,
               fname = fname)
  rp <- .get_json_from_post_query(api_url_signed_url, data)
  resp <- rp$resp
  parsed <- rp$parsed
  return(parsed)
}


####################### Helper functions

.get_json_from_post_query <- function(query, data){
  # returns api response and parsed output from POST query given data
  token <- .get_token()
  resp <- POST(api_url_signed_url, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  .check_response_status(resp, 200)
  
  return(list(resp=resp, parsed=parsed))
}