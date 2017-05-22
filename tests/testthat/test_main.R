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

test_that("test .obtain_task", {
  expect_equal(.obtain_task(c(1,0,0,0)), "bin_class")
  expect_equal(.obtain_task(c(1,2,3)), "reg")
})

test_that("test .data_check", {
  expect_error(.data_check(c(1,2,3), data.frame(a=c(1,2), b=c(2,1))),
               "Sorry, multiple outputs are not supported in MLJAR")
  expect_error(.data_check(as.data.frame(c(1,2,3)), data.frame(a=c(1,2))),
               "Sorry, there is a missmatch between X and y matrices shapes")
  expect_error(.data_check(as.data.frame(c(1,2)), data.frame(a=c(1,2))),
               NA)
})

test_that("test mljar_fit reactions to bad arguments",{
  expect_error(mljar_fit(x.tr, y.tr, validx=NULL, validy=NULL,
                         proj_title="fullproject1", exp_title="fullexp1",
                         algorithms = c(), metric = "logloss"),
               "You must specify non-empty vector of algorithms to use."
  )
})

test_that("test mljar_fit and mljar_predict integration test",{
  expname <- "fullexp1"
  bs <- mljar_fit(x.tr, y.tr, validx=x.vl, validy=y.vl,
                  proj_title="fullproject2", exp_title=expname,
                  algorithms = c("logreg"), metric = "logloss")
  expect_equal(bs$experiment, expname)
  expect_equal(bs$status, "Done")
  expect_error(predvals <- mljar_predict(bs, x.vl, "fullproject2"), NA)
  expect_equal(as.numeric(predvals>0.5), y.vl)
})

projects <- get_projects()
delete_project(projects$projects[[1]]$hid)
