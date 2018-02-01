library(testthat)
library(mljar)

gp <- get_projects()
if (length(gp$projects)>0) {
  for (pr in gp$projects){
    delete_project(pr$hid)
  }
}
test_check("mljar")
