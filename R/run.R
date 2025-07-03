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
  
  # if data.str if missing, assign an empty data.frame
  if(missing(data.struc)) data.struc <- data.frame()
  
  # pass data.str into shiny environment
  shiny_env <- 1
  envir = as.environment(shiny_env)
  assign("data.struc", data.struc, envir = envir)
  
  appDir <- system.file("shinySim", package = "shinySim")
  shiny::runApp(appDir, display.mode = "normal")
  
}


