library(mljar)
context("Test experiment")

pr_task <- "bin_class"
pr <- create_project('ds', pr_task, 'some description')
hid <- pr$hid

file_from_resources <- "binary_part_iris_converted.csv"
dataset1 <- add_dataset_if_not_exists(hid, file_from_resources, "test-exp1")

validation_kfolds <- 5
validation_shuffle <- TRUE
validation_stratify <-TRUE
validation_train_split <- NULL
validation <- "5-fold CV, Shuffle, Stratify"
algorithms <- c("logreg")
metric <- "logloss"
tuning_mode <- "Normal"
time_constraint <- 1
create_ensemble <- FALSE
dataset_preproc <- {}

exp <- add_experiment_if_not_exists(hid, dataset1$dataset, NULL, "exp",
                                    pr_task, validation_kfolds, validation_shuffle,
                                    validation_stratify, validation_train_split, algorithms, metric,
                                    tuning_mode, time_constraint, create_ensemble)
test_that("test get_results", {
  wait <- 5
  for (i in 1:wait){
    Sys.sleep(4) # wait till experiment is initiated
    exp_dd <- get_experiment(exp$hid)
    if (exp_dd$experiment$compute_now == 2) {
      r <- get_results(hid, exp$hid)
      expect_equal(length(r$results), 5)
      break
    }
  }
})

delete_project(hid)
