#' @title shinySim
#' @description shiny interface to the squidSim package. 
#' @return A simulated dataframe
#' @param data.struc datastructure argument created using squidSim or other.
#' @examples
#' \dontrun{
#' data_test <- squidSim::make_structure("sex(2)/individual(10)",
#' repeat_obs=2,level_names=list(sex=c("F","M")))
#' shinySim(data.struc = data_test)
#' }
#' @export

shinySim <- function(data.struc = NULL){
  #if data.struc is missing then label the data.struc object as missing (important for SD initation).
  #load into the environment the data.struc object and assign the UIs and Server into the env.
  if(missing(data.struc)){
    data.struc <- "Missing"
    shiny_env <- 1
    envir = as.environment(shiny_env)
    # print("Loading without specified data structure")
    assign("data.struc", data.struc, envir = envir)
  } else {
    #else if data.struc is not missing then label the data.struc object as the provided datapath (important for SS initation). 
    #otherwise same as previous
    data.struc <- data.struc
    shiny_env <- 1
    envir = as.environment(shiny_env)
    # print("Loading data structure")
    assign("data.struc", data.struc, envir = envir)
  }
  
appDir <- system.file("shinySim", package = "shinySim")
shiny::runApp(appDir, display.mode = "normal")
  
}


