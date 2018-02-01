library(mljar)
context("Test main")

file_from_resources <- "binary_part_iris_converted.csv"
irisdata <- read.csv(file_from_resources)
dx <- irisdata[-5]
dy <- irisdata[5]

irisdata2 <- irisdata[sample(nrow(irisdata)),]
x.tr <- irisdata2[1:80,-5]
y.tr <- irisdata2[1:80,5]
x.vl <- irisdata2[81:100,-5]
y.vl <- irisdata2[81:100,5]

expname <- "fullexp1"

test_that("test mljar_fit reactions to bad arguments",{
  expect_error(mljar_fit(NULL, NULL, validx=NULL, validy=NULL,
                         proj_title="fullproject1", exp_title="fullexp2",
                         algorithms = c("xgb"), metric = "logloss"),
               "NULL data"
  )
})

test_that("test mljar_fit and mljar_predict integration test",{
  bs <- mljar_fit(x.tr, y.tr, validx=x.vl, validy=y.vl,
                  proj_title="fullproject2", exp_title=expname,
                  algorithms = c("logreg"), metric = "logloss")
  expect_equal(bs$experiment, expname)
  expect_equal(bs$status, "Done")
  expect_error(predvals <- mljar_predict(bs, x.vl, "fullproject2"), NA)
  expect_equal(as.numeric(predvals > 0.5), y.vl)
  # test running predict with model id
  model_hid <- bs$hid
  expect_error(predvals <- mljar_predict(model_hid, x.vl, "fullproject2"), NA)
  expect_equal(as.numeric(predvals > 0.5), y.vl)
})

test_that("test get_all_models integration test",{
  expect_error(get_all_models("fullproject2", "x"),
     "MLJAR cannot find an experiment with such a title. Check and try again.")
  expect_error(get_all_models("f", "x"),
     "MLJAR cannot find a project with such a title. Check and try again.")
  df <- get_all_models("fullproject2", expname)
  expect_equal(colnames(df), c("hid", "model_type", "metric_value",
                               "metric_type", "validation_scheme"))
})

projects <- get_projects()
delete_project(projects$projects[[1]]$hid)
