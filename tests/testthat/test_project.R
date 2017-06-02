library(mljar)
context("Test API projects")

test_that("test get_projects", {
  gp <- get_projects()
  expect_equal(length(gp$projects), 0)
})

test_that("test create_project and get_projects", {
  task <- "bin_class"
  pr_a <- create_project("a", task, "description a")
  expect_match(pr_a$title, "a")
  pr_a <- create_project("b", task, "description b")
  expect_match(pr_a$title, "b")
  gp <- get_projects()
  expect_equal(length(gp$projects), 2)
})

test_that("test .verify_if_project_exists", {
  task <- "bin_class"
  .verify_if_project_exists
  expect_error(.verify_if_project_exists("a", task),
               "Project with the same title and task already exists, change name.")
})

test_that("test delete_project and get_projects", {
  gp <- get_projects()
  # here we search for project named a
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

test_that("test print_all_projects", {
  df <- print_all_projects()
  expect_equal(colnames(df), c("hid", "title", "task", "description"))
})

test_that("test get_project, delete_project and get_projects", {
  gp <- get_projects()
  hid <- gp$projects[[1]]$hid
  pr <- get_project(hid)
  expect_equal(pr$project$title, "b")
  expect_match(delete_project(hid), "succesfully deleted!")
  gp <- get_projects()
  expect_equal(length(gp$projects), 0)
})
