#' Get projects
#'
#' Gets list of available projects
#'
#' @return structure with parsed projects and http response
#' @export
get_projects <- function() {
  api_url_projects <- paste(MLAR_API_PATH, API_VERSION, "/projects" , sep="")
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

#' Print all projects
#'
#' Gives data.frame with basic information about existing projects
#'
#' @return data.frame with projects
#' @export
print_all_projects <- function() {
  projects <- get_projects()
  tmp_sa <- sapply(projects$projects,
                   function(x) c(x$hid, x$title, x$task, x$description),
                   simplify = FALSE, USE.NAMES = TRUE)
  df_proj <- t(as.data.frame(tmp_sa,
                            row.names = c("hid", "title",
                                          "task", "description"),
                            col.names = 1:length(tmp_sa)))
  return(df_proj)
}

#' Get project
#'
#' Get data from a project of specified hid
#'
#' @param hid character with project unique identifier
#'
#' @return structure with parsed project and http response
#' @export
get_project <- function(hid) {
  api_url_project_hid <- paste(MLAR_API_PATH, API_VERSION, "/projects/", hid, sep="")
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

#' Creates a new project
#'
#' @param title character with project title
#' @param task character with project task
#' @param description optional description
#'
#' @return project details structure
#' @export
create_project <-function(title, task, description=""){
  .verify_if_project_exists(title, task)
  token <- .get_token()
  api_url_projects <- paste(MLAR_API_PATH, API_VERSION, "/projects" , sep="")
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
    print(sprintf("Project <%s> succesfully created!", title))
  }
  project_details <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  return(project_details)
}

#' Delete project
#'
#' @param hid charceter with project identifier
#'
#' @export
#' @importFrom httr DELETE status_code
delete_project <-function(hid){
  token <- .get_token()
  api_url_project_hid <- paste(MLAR_API_PATH, API_VERSION, "/projects/", hid, sep="")
  resp <- DELETE(api_url_project_hid, add_headers(Authorization = paste("Token", token)))
  if (status_code(resp)==204 || status_code(resp)==200){
    print(sprintf("Project <%s> succesfully deleted!", hid))
  }
}

# Helper project functions

#' Verify_if_project_exists
#'
#' Checks if there is no project with the same name and task.
#'
#' @param projtitle character with project title
#' @param task characeter with project task
#'
#' @return TRUE if okay, stops if such a project exists.
.verify_if_project_exists <- function(projtitle, task){
  gp <- get_projects()
  for (proj in gp$projects){
    if (proj$title==projtitle && proj$task==task){
      stop("Project with the same title and task already exists, change name.")
    }
  }
  return(TRUE)
}
