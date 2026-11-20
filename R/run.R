#' Build a squidSim simulation in a point-and-click app
#'
#' @description
#' Opens the shinySim app, a point-and-click interface to the
#' [squidSim](https://github.com/squidgroup/squidSim) package for simulating
#' hierarchical (multilevel) data. Starting from a data structure, you add
#' predictors, fixed effects, interactions and variance components at each
#' level, and the app builds the matching squidSim `parameters` list for you.
#'
#' As you go, the app shows:
#' * the model equation being simulated;
#' * the expected mean and variance of the response, and how the variance
#'   divides among levels and predictors;
#' * a check from a quick simulation, with a warning if its variance or mean
#'   differs noticeably from the expected values;
#' * the squidSim code, with a button to copy it into R.
#'
#' Paste that code into R and run [squidSim::simulate_population()] to
#' generate datasets. Nothing is uploaded anywhere: the app runs locally.
#'
#' @param data.struc Optional data structure to build the simulation on: a data
#'   frame of grouping factors, one column per level and one row per
#'   observation, usually made with [squidSim::make_structure()]. If `NULL`
#'   (the default), the app starts without one and the code it gives uses
#'   a sample size (`n =`) instead of `data_structure =`.
#' @return Called for its side effect of running the app; returns nothing
#'   useful. The simulation code is copied out of the app.
#' @seealso [squidSim::make_structure()] to make a data structure,
#'   [squidSim::simulate_population()] to simulate data with the code from the
#'   app.
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
