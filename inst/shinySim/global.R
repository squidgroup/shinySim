#### Functions ####
source("./functions/equation_writing.R",local=TRUE)
source("./functions/simulated_variances.R",local=TRUE)

#### Stopping the app ####
# The app stops when its last browser tab is closed. A page refresh also ends the session, so wait a
# few seconds before stopping: the refreshed page opens a new session in that time and the app keeps
# running. Set shiny::shinyOptions(shinySim.stop_on_close = FALSE) to keep the app running regardless.
open_sessions <- new.env()
open_sessions$n <- 0
session_opened <- function() {
  open_sessions$n <- open_sessions$n + 1
}
session_closed <- function(grace = 5) {
  open_sessions$n <- open_sessions$n - 1
  if (isTRUE(shiny::getShinyOption("shinySim.stop_on_close", default = TRUE))) {
    later::later(function() {
      if (open_sessions$n <= 0) shiny::stopApp()
    }, delay = grace)
  }
}
