library(mljar)
context("Test main")

irisdata <- read.csv("resources/binary_part_iris_converted.csv")
dx <- irisdata[-5]
dy <- irisdata[5]

test_that("test .obtain_task", {
  expect_equal(.obtain_task(c(1,0,0,0)), "bin_class")
  expect_equal(.obtain_task(c(1,2,3)), "reg")
})

test_that("test .data_check", {
  expect_error(.data_check(c(1,2,3),data.frame(a=c(1,2), b=c(2,1))),
               "Sorry, multiple outputs are not supported in MLJAR")
  expect_error(.data_check(as.data.frame(c(1,2,3)),data.frame(a=c(1,2))),
               "Sorry, there is a missmatch between X and y matrices shapes")
  expect_error(.data_check(as.data.frame(c(1,2)),data.frame(a=c(1,2))),
               NA)
})

test_that("test mljar_fit reactions to bad arguments",{
  expect_error(mljar_fit(dx, dy, validx=NULL, validy=NULL,
                         proj_title="fullproject1", exp_title="fullexp1",
                         algorithms = c(), metric = "logloss"),
               "You must specify non-empty vector of algorithms to use."
  )
  expect_error(mljar_fit(dx, NULL, validx=NULL, validy=NULL,
                         proj_title="fullproject1", exp_title="fullexp1",
                         algorithms = c("xgb"), metric = "logloss"),
               "NULL data"
  )
})


mljar_fit(dx, dy, validx=NULL, validy=NULL,
          proj_title="fullproject1", exp_title="fullexp1",
          algorithms = c("xgb"), metric = "logloss")
