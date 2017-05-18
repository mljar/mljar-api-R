#' Get results statistics
.get_results_stats <- function(results){
  resstats <- list()
  resstats$initiated_cnt <- 0
  resstats$learning_cnt  <- 0
  resstats$done_cnt      <- 0
  resstats$error_cnt     <- 0
  for (r in results$results){
    if (r$status == "Initiated"){
      resstats$initiated_cnt = resstats$initiated_cnt + 1
    } else if (r$status == "Learning"){
      resstats$learning_cnt = resstats$learning_cnt + 1
    } else if (r$status == "Done"){
      resstats$done_cnt = resstats$done_cnt + 1
    } else {
      resstats$error_cnt = resstats$error_cnt + 1
    }
  }
  return(resstats)
}

#' checks if data is in good format
.data_check <- function(x, y){
  if(length(dim(y))>1 && dim(x)[2]>1){
    stop("Sorry, multiple outputs are not supported in MLJAR")
  }
  if(dim(y)[1]!=dim(x)[1]){
    stop("Sorry, there is a missmatch between X and y matrices shapes")
  }
}

#' converts data to temporary file
.data_to_file <- function(x, y){
  # first we check if data is valid
  .data_check(x, y)
  # now it's time to convert to data frame
  dataxy <- as.data.frame(x)
  dataxy["target"] <- y
  tmpfilepath <- tempfile()
  file.create(tmpfilepath)
  write.csv(dataxy, file = tmpfilepath, row.names = F)
  return(tmpfilepath)
}

# determines what kind of task is that basing on y
.obtain_task <- function(y){
  return(length(unique(y))>2, "reg", "bin_class")
}

# starts experiment
.start_experiment <- function(x, y, valid=NULL){
  task <- .obtain_task(y)

}

mljar_fit <- function(){

}

mljar_predict <- function(){

}
