library(mljar)
context("Test API dataset")

task <- "bin_class"
pr <- create_project("ds", task, "some description")
hid <- pr$hid


test_that("test add_new_dataset", {
  file_from_resources <- system.file("resources/binary_part_iris_converted.csv", package = "mljar")
  expect_error(add_new_dataset(hid, file_from_resources, "title"), NA)
  Sys.sleep(3) # to ensure that project is created
})


test_that("test get_datasets", {
  ds <- get_datasets(hid)
  expect_equal(length(get_datasets(hid)$datasets), 1)
})

test_that("test get_dataset", {
  ds_hid <- get_datasets(hid)$datasets[[1]]$hid
  expect_equal(get_dataset(ds_hid)$dataset$hid, ds_hid)
})

test_that("test .wait_till_all_datasets_are_valid", {
  expect_true(.wait_till_all_datasets_are_valid(hid))
})

test_that("test .accept_dataset_column_usage", {
  ds_hid <- get_datasets(hid)$datasets[[1]]$hid
  expect_true(.accept_dataset_column_usage(ds_hid))
})

test_that("test delete_dataset", {
  ds_hid <- get_datasets(hid)$datasets[[1]]$hid
  delete_dataset(ds_hid)
  expect_equal(length(get_datasets(hid)$datasets), 0)
})

test_that( "test add_dataset_if_not_exists", {
  file_from_resources <- system.file("resources/binary_part_iris_converted.csv", package = "mljar")
  expect_error(add_dataset_if_not_exists(hid, file_from_resources, "title-1"), NA)
  expect_error(add_dataset_if_not_exists(hid, file_from_resources, "title-1"), "Dataset with the same name already exists")
})

delete_project(hid)

