library(testthat)
library(mljar)

# token needed by CRAN
Sys.setenv(MLJAR_TOKEN="10bc57e737c2ca5516bb01ab29549978b53d83a4")
gp <- get_projects()
if (length(gp$projects)>0) {
  for (pr in gp$projects){
    delete_project(pr$hid)
  }
}
test_check("mljar")
Sys.unsetenv("MLJAR_TOKEN")
