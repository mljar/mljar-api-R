#' Uploads file into MLJAR
#'
#' It uploads file into MLJAR and returns destination path.
#'
#' @param project_hid character with project identifier
#' @param filepath character with path to file
#'
#' @return character with destination path
#'
#' @importFrom httr PUT
#' @export
upload_file <- function(project_hid, filepath){

  parsed <- .get_signed_url(project_hid, filepath)
  signed_url <- parsed$signed_url
  dst_path <- parsed$destination_path
  plain_text_data <- readr::read_file(filepath)
  resp <- PUT(signed_url, body=plain_text_data)
  .check_response_status(resp, 200, "Upload into MLJAR failed")
  return(dst_path)
}

#' Get signed url
#'
#' From given project hid and filepath returns signed url for uploading.
#'
#' @param project_hid character with project identifier
#' @param filepath path to the file
#'
#' @return parsed htt response from MLJAR s3policy (check mljar api for more)
.get_signed_url <- function(project_hid, filepath){
  api_url_signed_url <- paste(MLAR_API_PATH, API_VERSION, "/s3policy/" , sep="")
  fname = tail(strsplit(filepath, "/")[[1]], n=1)
  data <- list(project_hid = project_hid,
               fname = fname)
  rp <- .get_json_from_post_query(api_url_signed_url, data)
  resp <- rp$resp
  parsed <- rp$parsed
  return(parsed)
}

