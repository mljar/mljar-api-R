library(mljar)
context("Test utils")

test_that("test .get_token", {
  tok <- .get_token()
  expect_type(tok, "character")
})

test_that("test .get_json_from_get_query", {
  query <- paste0("https://mljar.com/api/", API_VERSION, "/projects")
  r <- .get_json_from_get_query(query)
  expect_equal(names(r), c("resp", "parsed"))
})

test_that("test .check_response_status", {
  query <- paste0("https://mljar.com/api/", API_VERSION, "/projects")
  r <- .get_json_from_get_query(query)
  expect_error(.check_response_status(r$resp, 200), NA)
  expect_error(.check_response_status(r$resp, 222, "omg"), "omg")
})

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

test_that("test .data_to_file", {
  tmpf <- .data_to_file(c(1,2))
  expect_type(tmpf, "character")
  expect_equal(unlist(strsplit(tmpf,"[.]"))[[2]], "csv")
})
