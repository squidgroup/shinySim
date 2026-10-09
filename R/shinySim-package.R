#' @keywords internal
"_PACKAGE"

## The app itself lives in inst/shinySim and calls these packages with `::`; they are imported here
## so R CMD check can see that they are used.
#' @importFrom bs4Dash dashboardPage
#' @importFrom DT datatable
#' @importFrom fresh create_theme
#' @importFrom graphics barplot
#' @importFrom grDevices hcl.colors
#' @importFrom later later
#' @importFrom prompter use_prompt
#' @importFrom shiny runApp
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput
#' @importFrom squidSim simulate_population
#' @importFrom stats var
NULL
