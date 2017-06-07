[![Build Status](https://travis-ci.org/mljar/mljar-api-R.svg?branch=master)](https://travis-ci.org/mljar/mljar-api-R)
[![codecov](https://codecov.io/gh/mljar/mljar-api-R/branch/master/graph/badge.svg)](https://codecov.io/gh/mljar/mljar-api-R)

# mljar-api-R

A simple R wrapper for **mljar.com** API. It allows MLJAR users to create Machine Learning models with few lines of code:

```R
library(mljar)

model <- mljar_fit(x.training, y.training, validx=x.validation, validy=y.validation,
                proj_title="Project title", exp_title="experiment title",
                algorithms = c("logreg"), metric = "logloss")

predicted_values <- mljar_predict(model, x.to.predict, "Project title")
```

That's all folks! Yeah, I know, this makes Machine Learning super easy! You can use this code for following Machine Learning tasks:
 * Binary classification (your target has only two unique values)
 * Regression (your target value is continuous)
 * More is coming soon!

## How to install

You can install mljar directly from **CRAN**:

    install.packages("mljar")

Alternatively, you can install the latest development version from GitHub using `devtools`:

    devtools::install_github("mljar/mljar-api-R")

## How to use it

 1. Create an account at mljar.com and login.
 2. Please go to your users settings (top, right corner).
 3. Get your token, for example 'exampleexampleexample'.
 4. Set environment variable `MLJAR_TOKEN` with your token value in shell:
```
export MLJAR_TOKEN=exampleexampleexample
```
or directly in RStudio:
```
Sys.setenv(MLJAR_TOKEN="examplexampleexample")
```

 5. That's all, you are ready to use MLJAR in your R code!

## What's going on?

 * This wrapper allows you to search through different Machine Learning algorithms and tune each of the algorithm.
 * By searching and tuning ML algorithm to your data you will get very accurate model.
 * By calling function `mljar_fit` you create new project and start experiment with models training.
 All your results will be accessible from your mljar.com account - this makes Machine Learning super easy and
 keeps all your models and results in beautiful order. So, you will never miss anything.
 * All computations are done in MLJAR Cloud, they are executed in parallel. So after calling `mljar_fit` method you can switch
 your computer off and MLJAR will do the job for you!
 * I think this is really amazing! What do you think? Please let us know at `contact@mljar.com`.

## Examples

Soon

## Testing

To run tests use simple command in your R session:

```R
devtools::test()
```
