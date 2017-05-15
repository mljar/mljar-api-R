library(mljar)
context("Test API dataset")

task <- "Binary Classification"
create_project('ds', task, 'some description')
gp <- get_projects()
for(i in 1:length(gp$projects)) {
  if (gp$projects[[i]]$title=="ds"){
    break
  }
}
hid <- gp$projects[[i]]$hid


test_that("test add_new_dataset", {
  file_from_resources <- "resources/binary_part_iris_converted.csv"
  expect_error(add_new_dataset(hid, file_from_resources, "title"), NA)
  Sys.sleep(5) # to ensure that project is created
})


test_that("test get_datasets", {
  ds <- get_datasets(hid)
  expect_equal(length(get_datasets(hid)$datasets), 1)
})

test_that("test get_dataset", {
  ds_hid <- get_datasets(hid)$datasets[[1]]$hid
  expect_equal(get_dataset(ds_hid)$dataset$hid, ds_hid)
})

test_that("test delete_dataset", {
  ds_hid <- get_datasets(hid)$datasets[[1]]$hid
  delete_dataset(ds_hid)
  expect_equal(length(get_datasets(hid)$datasets), 0)
})

delete_project(hid)
