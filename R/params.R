
# MLJAR Constants
#################

API_VERSION <- "v1"

MLJAR_TASKS <- list( bin_class = 'Binary Classification',
                     regression = 'Regression'
                     )

MLJAR_METRICS  <- list(auc = 'Area Under Curve',
                       logloss = 'Logarithmic Loss',
                       rmse = 'Root Mean Square Error',
                       mse = 'Mean Square Error',
                       mae = 'Mean Absolute Error')

MLJAR_DEFAULT_FOLDS = 5
MLJAR_DEFAULT_SHUFFLE = TRUE
MLJAR_DEFAULT_STRATIFY = TRUE
MLJAR_DEFAULT_TRAIN_SPLIT = NULL

MLJAR_BIN_CLASS  <- list(xgb = 'Extreme Gradient Boosting',
                         lgb = 'LightGBM',
                         rfc = 'Random Forest',
                         rgfc = 'Regularized Greedy Forest',
                         etc = 'Extra Trees',
                         knnc = 'k-Nearest Neighbor',
                         logreg = 'Logistic Regression',
                         mlp = 'Neural Network'
                         )

MLJAR_REGRESSION   <- list(xgbr = 'Extreme Gradient Boosting',
                           lgbr = 'LightGBM',
                           rfr = 'Random Forest',
                           rgfr = 'Regularized Greedy Forest',
                           etr = 'Extra Trees'
                           )

MLJAR_TUNING_MODES <- list(Normal = list(random_start_cnt = 5, hill_climbing_cnt = 1),
                           Sport = list(random_start_cnt = 10, hill_climbing_cnt = 2),
                           Insane = list(random_start_cnt = 15, hill_climbing_cnt = 3)
                           )

# MLJAR Defaults
#################

MLJAR_DEFAULT_METRICS  <- list(bin_class = 'logloss',
                               regression = 'rmse')

MLJAR_DEFAULT_ALGORITHMS <- list( bin_class = c('xgb', 'lgb', 'mlp'),
                                  regression = c('xgbr', 'lgbr')
                                )

MLJAR_DEFAULT_ENSEMBLE        = TRUE
MLJAR_DEFAULT_TUNING_MODE     = 'Normal'
MLJAR_DEFAULT_TIME_CONSTRAINT = '5' # minutes

MLJAR_OPT_MAXIMIZE = c('auc')
