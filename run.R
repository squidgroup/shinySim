shinySim <- function(data.struc = NULL){
  #if data.struc is missing then label the data.struc object as missing (important for SD initation).
  #load into the environment the data.struc object and assign the UIs and Server into the env.
  if(missing(data.struc)){
    data.struc <- "Missing"
    shiny_env <- 1
    envir = as.environment(shiny_env)
    print("Loading without specified data structure")
    assign("data.struc", data.struc, envir = envir)
  } else {
    #else if data.struc is not missing then label the data.struc object as the provided datapath (important for SS initation). 
    #otherwise same as previous
    data.struc <- data.struc
    shiny_env <- 1
    envir = as.environment(shiny_env)
    print("Loading data structure")
    assign("data.struc", data.struc, envir = envir)
  }
  
  shiny::runApp()
  
}

source("equation_writing.R")

data_test <- data.frame(HELLO = 1:5, Joel = rnorm(5,6,7))
shinySim(data.struc = data_test)
