library(mljar)
context("Test API data upload")

test_that("test data_upload", {
  task <- "Binary Classification"
  create_project('a', task, 'some description')
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
