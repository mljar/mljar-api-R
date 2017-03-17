get_projects <- function() {
  #' Gets list of available projects
  api_version <- "v1"
  api_url_projects <- paste("https://mljar.com/api/", api_version, "/projects" , sep="")
  rp <- .get_json_from_query(api_url_projects)
  resp <- rp$resp
  parsed <- rp$parsed
  
  structure(
    list(
      projects = parsed,
      response = resp
    ),
    class = "get_projects"
  )
}

print.get_projects <- function(x, ...) {
  cat("<MLJAR projects >\n", sep = "")
  str(x$projects)
  invisible(x)
}

get_project <- function(hid) {
  #' Get data from a project of specified hid
  api_version <- "v1"
  api_url_project_hid <- paste("https://mljar.com/api/", api_version, "/projects/", hid, sep="")
  rp <- .get_json_from_query(api_url_project_hid)
  resp <- rp$resp
  parsed <- rp$parsed

  structure(
    list(
      project = parsed,
      response = resp
    ),
    class = "get_project"
  )
}

print.get_project <- function(x, ...) {
  cat("<MLJAR project >\n", sep = "")
  str(x$project)
  invisible(x)
}

create_project <-function(title, task, description=''){
  #' creates project
  token <- .get_token()
  api_url_projects <- paste("https://mljar.com/api/", api_version, "/projects" , sep="")
  data <- list(hardware = 'cloud',
               scope = 'private',
               task = task,
               compute_now = 0,
               description = description)
  resp <- POST(api_url_projects, add_headers(Authorization = paste("Token", token)),
       body = data, encode = "json")
  .check_response_status(resp, 201)
  }

####################### Helper functions

.get_json_from_query <- function(query){
  # returns api response and parsed output
  token <- .get_token()
  resp <- GET(query, add_headers(Authorization = paste("Token", token)))
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  .check_response_status(resp, 200)

    return(list(resp=resp, parsed=parsed))
}

.get_token <- function(){
  token <- Sys.getenv("MLJAR_TOKEN")
  if (nchar(token)==0) {
    stop("Specify MLJAR_TOKEN env variable", call. = FALSE)
  }
  return(token)
}

.check_response_status(resp, expected_code){
  if (status_code(resp) != expected_code) {
    stop(
      sprintf(
        "MLJAR API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
}