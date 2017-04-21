
upload_file <- function(projecthid, filepath){
  # uploads file into MLJAR and returns destination path
  parsed <- .get_signed_url(projecthid, filepath)
  signed_url <- parsed$signed_url
  dst_path <- parsed$destination_path
  plain_text_data <- readr::read_file(filepath)
  resp <- PUT(signed_url, body=plain_text_data)
  .check_response_status(resp, 200, "Upload into MLJAR failed")
  return(dst_path)
}

.get_signed_url <- function(projecthid, filepath){
  # from given project hid and filepath returns signed url for uploading
  api_url_signed_url <- paste("https://mljar.com/api/", API_VERSION, "/s3policy/" , sep="")
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
  resp <- POST(query, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  .check_response_status(resp, 200)

  return(list(resp=resp, parsed=parsed))
}
