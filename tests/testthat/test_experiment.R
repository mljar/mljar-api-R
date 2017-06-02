library(mljar)
context("Test experiment")

task <- "bin_class"
pr <- create_project('ds', task, 'some description')
hid <- pr$hid

pr_task <- pr$task
file_from_resources <- "binary_part_iris_converted.csv"
dataset1 <- add_dataset_if_not_exists(hid, file_from_resources, "test-exp1")

validation_kfolds <- 5
validation_shuffle <- TRUE
validation_stratify <-TRUE
validation_train_split <- NULL
validation <- "5-fold CV, Shuffle, Stratify"
algorithms <- c("xgb")
metric <- "logloss"
tuning_mode <- "Normal"
time_constraint <- 1
create_ensemble <- FALSE
dataset_preproc <- {}

test_that("test create_experiment", {
  params <- list(
    train_dataset = list(id = dataset1$dataset$hid, title = dataset1$dataset$title),
    algs = c(algorithms,""),
    preproc = dataset_preproc,
    single_limit = time_constraint,
    ensemble = create_ensemble,
    random_start_cnt = MLJAR_TUNING_MODES[[tuning_mode]][["random_start_cnt"]],
    hill_climbing_cnt =  MLJAR_TUNING_MODES[[tuning_mode]][["hill_climbing_cnt"]]
  )
  params <- jsonlite::toJSON(params, auto_unbox =TRUE)
  exp_data <- list( title =  "exp-1",
                    description = "",
                    metric = metric,
                    validation_scheme = validation,
                    task = pr_task,
                    compute_now = 1,
                    parent_project = hid,
                    params = params
                  )
  expect_error(create_experiment(exp_data), NA)

})

test_that("test get_experiments", {
  ds <- get_experiments(hid)
  expect_equal(length(get_experiments(hid)$experiments), 1)
})

test_that("test get_experiment", {
  ex_hid <- get_experiments(hid)$experiments[[1]]$hid
  expect_equal(get_experiment(ex_hid)$experiment$hid, ex_hid)
})

test_that("test add_experiment_if_not_exists", {
  expect_error(add_experiment_if_not_exists(hid, dataset1$dataset, NULL, "exp-2",
                               pr_task, validation_kfolds, validation_shuffle,
                               validation_stratify, validation_train_split, algorithms, metric,
                               tuning_mode, time_constraint, create_ensemble), NA)

  expect_error(add_experiment_if_not_exists(hid, dataset1$dataset, NULL, "exp-2",
                               pr_task, validation_kfolds, validation_shuffle,
                               validation_stratify, validation_train_split, algorithms, metric,
                               tuning_mode, time_constraint, create_ensemble),
               "Please rename your new experiment with new parameters setup.")
})

delete_project(hid)
