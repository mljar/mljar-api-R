library(mljar)
context("Test API projects")

.get_token <- function(){
  # returns token of test account
  return("10bc57e737c2ca5516bb01ab29549978b53d83a4")
}

test_that("test get_projects", {
  gp <- get_projects()
  expect_equal(length(gp$projects), 0)
})

test_that("test create_project and get_projects", {
  task <- "Binary Classification"
  expect_match(create_project('a', task,'description a'), "Project 'a' succesfully created!")
  expect_match(create_project('b', task,'description b'), "Project 'b' succesfully created!")
  gp <- get_projects()
  expect_equal(length(gp$projects), 2)
})

test_that("test delete_project and get_projects", {
  gp <- get_projects()
  for(i in 1:length(gp$projects)) {
    if (gp$projects[[i]]$title=="a"){
      break
    }
  }
  hid <- gp$projects[[i]]$hid
  expect_match(delete_project(hid), "succesfully deleted!")
  gp <- get_projects()
  expect_equal(length(gp$projects), 1)
})

test_that("test get_project and delete_project", {
  gp <- get_projects()
  hid <- gp$projects[[1]]$hid
  pr <- get_project(hid)
  expect_equal(pr$project$title, "b")
  expect_match(delete_project(hid), "succesfully deleted!")
  gp <- get_projects()
  expect_equal(length(gp$projects), 0)
})
