library(mljar)
context("Test experiment")

test_that("test create_experiment", {
  task <- "Binary Classification"
  create_project('a', task, 'some description')
  gp <- get_projects()
  tmpfilepath <- tempfile()
  file.create(tmpfilepath)
  write.csv(c(1.0,2.0,1.1), file = tmpfilepath)
  project_hid <- gp$projects[[1]]$hid
  up <- upload_file(project_hid, tmpfilepath)

})

test_that("test get_experiment", {
})

test_that("test get_experiments", {
})


