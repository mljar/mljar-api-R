library("RCurl")
library("rjson")

token <- "510145da007dfb92c6395d4cce441d8cc7e7e868"
resp <- getURL("https://mljar.com/api/v1/projects", 
               httpheader = c(Authorization = paste("Token", token)))
json_data <- fromJSON(resp)
for (i in 1:length(json_data)){
  print(json_data[[i]]$title)
}

#json_data[[1]]$title
#json_data[[1]]$title
