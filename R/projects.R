get_projects <- function() {
  #' Gets list of available projects
  api_url_projects <- paste("https://mljar.com/api/", API_VERSION, "/projects" , sep="")
  rp <- .get_json_from_get_query(api_url_projects)
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
  api_url_project_hid <- paste("https://mljar.com/api/", API_VERSION, "/projects/", hid, sep="")
  rp <- .get_json_from_get_query(api_url_project_hid)
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

# checks if there is no project with the same name and task
.verify_if_project_exists <- function(projtitle, task){
  gp <- get_projects()
  for (proj in gp$projects){
    if (proj$title==projtitle && proj$task==task){
      stop("Project with the same title and task already exists, change name.")
    }
  }
  return(TRUE)
}


create_project <-function(title, task, description=''){
  #' creates a new project
  .verify_if_project_exists(title, task)
  token <- .get_token()
  api_url_projects <- paste("https://mljar.com/api/", API_VERSION, "/projects" , sep="")
  data <- list(title = title,
               hardware = 'cloud',
               scope = 'private',
               task = task,
               compute_now = 0,
               description = description)
  resp <- POST(api_url_projects, add_headers(Authorization = paste("Token", token)),
               body = data, encode = "form")
  .check_response_status(resp, 201)
  if (status_code(resp)==201){
    sprintf("Project '%s' succesfully created!", title)
  }
  project_details <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  return(project_details)
}

delete_project <-function(hid){
  #' deletes project
  token <- .get_token()
  api_url_project_hid <- paste("https://mljar.com/api/", API_VERSION, "/projects/", hid, sep="")
  resp <- DELETE(api_url_project_hid, add_headers(Authorization = paste("Token", token)))
  if (status_code(resp)==204 || status_code(resp)==200){
    sprintf("Project <%s> succesfully deleted!", hid)
  }
}

####################### Helper functions

.get_json_from_get_query <- function(query){
  # returns api response and parsed output
  token <- .get_token()
  resp <- GET(query, add_headers(Authorization = paste("Token", token)))
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  .check_response_status(resp, 200)

  return(list(resp=resp, parsed=parsed))
}

.get_token <- function(){
  # returns token defined in enviromental variables
  token <- Sys.getenv("MLJAR_TOKEN")
  if (identical(token, "")) {
    stop("Specify MLJAR_TOKEN env variable", call. = FALSE)
  }
  return(token)
}

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
