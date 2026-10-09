#' @title shinySim
#' @description shiny interface to the squidSim package.
#' @return Called for its side effect of running the app, which shows the squidSim code for the
#'   simulation built in it (to copy into R) and the expected mean and variance of the response.
#' @param data.struc datastructure argument created using squidSim or other.
#' @examples
#' \dontrun{
#' data_test <- squidSim::make_structure("sex(2)/individual(10)",
#' repeat_obs=2,level_names=list(sex=c("F","M")))
#' shinySim(data.struc = data_test)
#' }
#' @export

shinySim <- function(data.struc = NULL){

  # if data.struc is missing, use an empty data.frame
  if(is.null(data.struc)) data.struc <- data.frame()

  # pass data.struc to the app as a shiny option (read by the server), not via the global environment
  shiny::shinyOptions(shinySim.data.struc = as.data.frame(data.struc))
  on.exit(shiny::shinyOptions(shinySim.data.struc = NULL), add = TRUE)

  appDir <- system.file("shinySim", package = "shinySim")
  shiny::runApp(appDir, display.mode = "normal")

}
