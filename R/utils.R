# MLJAR Helper Functions

#' Get json from post query
#'
#' Returns api response and parsed output from POST query given data
#'
#' @param query character with http query
#' @param data list with body data
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite fromJSON
#' @return list with response and parsed response from json
.get_json_from_post_query <- function(query, data){
  token <- .get_token()
  resp <- POST(query, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  .check_response_status(resp, 200)
  return(list(resp=resp, parsed=parsed))
}

#' Get json from get query
#'
#' Returns api response and parsed output
#'
#' @param query character with http query
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @return list with response and parsed response from json
.get_json_from_get_query <- function(query){
  token <- .get_token()
  resp <- GET(query, add_headers(Authorization = paste("Token", token)))
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  .check_response_status(resp, 200)

  return(list(resp=resp, parsed=parsed))
}

#' Get token
#'
#' Gets token from MLJAR_TOKEN env variable.
#'
#' @return returns token defined in enviromental variable MLJAR_TOKEN
#' @examples
#' .get_token()
.get_token <- function(){
  token <- Sys.getenv("MLJAR_TOKEN")
  if (identical(token, "")) {
    stop("Specify MLJAR_TOKEN env variable", call. = FALSE)
  }
  return(token)
}

#' Check response status
#'
#' Verifies if response status is correct.
#' If not it stops execution with message.
#'
#' @param resp httr response
#' @param expected_code numeric with expected code e.g. 201
#' @param error_message character with error message
#'
#' @importFrom httr status_code
.check_response_status <- function(resp, expected_code,
                                   error_message="MLJAR API request failed"){
  # compares response status with expeced_code and returns error_message if not equal
  if (status_code(resp) != expected_code) {
    stop(
      sprintf(
        paste(error_message, "[%s]\n"),
        status_code(resp)
      ),
      call. = FALSE
    )
  }
}

#' Checks if data is in good format.
#'
#' If not it stops execution.
#'
#' @param x preferably a matrix or data frame.
#' If not, it is attempted to coerce x to a data frame.
#' @param y preferably a matrix or data frame.
#' If not, it is attempted to coerce x to a data frame.
.data_check <- function(x, y){
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  if (is.null(x) || is.null(y)){
    stop("NULL data")
  }
  if(length(dim(y))>1 && dim(y)[2]>1){
    stop("Sorry, multiple outputs are not supported in MLJAR")
  }
  if(dim(y)[1]!=dim(x)[1]){
    stop("Sorry, there is a missmatch between X and y matrices shapes")
  }
}

#' Stores data in temporary CSV file
#'
#' @param x preferably a matrix or data frame.
#' If not, it is attempted to coerce x to a data frame.
#' @param y preferably a matrix or data frame.
#' If not, it is attempted to coerce x to a data frame.
#'
#' @return tmpfilepath character with path to temporary file
#'
#' @example
#' .data_to_file(c(1,2))
.data_to_file <- function(x, y=NULL){
  if (!is.null(y)){
    # first we check if data is valid
    .data_check(x, y)
    # now it's time to convert to data frame
    dataxy <- as.data.frame(x)
    dataxy["target"] <- y
  } else {
    if (is.null(x)) stop("NULL data")
    dataxy <- as.data.frame(x)
  }
  # temporary csv file is created
  tmpfilepath <- paste0(tempfile(),".csv")
  file.create(tmpfilepath)
  write.csv(dataxy, file = tmpfilepath, row.names = F)
  return(tmpfilepath)
}

#' Obtain task
#'
#' Determines what kind of task is that basing on y.
#' @param y target vector/data.frame
#'
#' @return "reg" or "bin_class" depending on kind of task
#' @examples
#' .obtain_task(c(1,0,0,1))
.obtain_task <- function(y){
  return(ifelse(length(unique(y))>2, "reg", "bin_class"))
}
