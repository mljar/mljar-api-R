library(testthat)
library(mljar)

.get_token <- function(){
  # returns token of test account
  return("10bc57e737c2ca5516bb01ab29549978b53d83a4")
}

Sys.setenv(MLJAR_TOKEN="10bc57e737c2ca5516bb01ab29549978b53d83a4")ś
test_check("mljar")
Sys.unsetenv(MLJAR_TOKEN)
