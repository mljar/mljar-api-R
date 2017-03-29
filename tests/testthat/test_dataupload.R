library(mljar)
context("Test API projects")

.get_token <- function(){
  # returns token of test account
  return("10bc57e737c2ca5516bb01ab29549978b53d83a4")
}

test_that("test data_upload", {
  task <- "Binary Classification"
  create_project('a', task,'description a')
  gp <- get_projects()
  tmpfilepath <- tempfile()
  file.create(tmpfilepath)
  write.csv(c(1.0,2.0,1.1), file = tmpfilepath)
  project_hid <- gp$projects[[1]]$hid
  up <- upload_file(project_hid, tmpfilepath)
  expect_gt(nchar(up), 1)
  delete_project(project_hid)
  file.remove(tmpfilepath)
})
