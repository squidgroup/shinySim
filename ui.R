# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

ui <- function() {
  shinydashboard::dashboardPage(
    shinyjs::useShinyjs(),
    header = shinydashboard::dashboardHeader(
      title = tags$img(src = "squidSim_logo.png", height = "80", width = "80")
    ),
    sidebar = shinydashboard::dashboardSidebar(
      width = 300,
      shiny::br(),
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Data Structure",
        
        shiny::textInput("input_structure", label = NULL)
      ),
      
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Add Component",
        
        shinyWidgets::pickerInput(
          inputId = "input_group",
          label = NULL,
          selected = NULL,
          choices = c("observation", "interactions", "c"),
          options = list(
            title = "Group"
          )
        ),
        
        shinyjs::hidden(
          shiny::div(
            id = "group_name",
            shiny::textInput("input_group_name",
              label = shiny::tags$span(style = "color: grey;", "Group Name (optional)")
            )
          )
        ),
        
        shiny::numericInput(
          inputId = "input_variable_no",
          label = shiny::tags$span(style = "color: black;", "Number of Variables"),
          1, min = 1, max = 10
        ),
        shinyjs::hidden(
          shiny::div(
            id = "component_type",
        shinyWidgets::pickerInput(
          inputId = "input_component",
          label = NULL,
          choices = c(
            "predictor", "random", "fixed categorical",
            "covariate"
          ),
          options = list(
            title = "Component type"
          )
        )
          )
        )
        
        shinyjs::hidden(
          shiny::div(
            id = "parameter_table",
            
        
      ),

      # depending on each component different things come up.
      # Hidden boxes - four things. little tables.
      # names - always comes up, depends on numerinput
      # names, mean and beta, number of elements equal to input variable number.
      # vcov, little matrix (n by n).
      # radio button with covariance or correlation.

      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Edit Component",
        shinyWidgets::pickerInput(
          inputId = "edit_component",
          label = NULL,
          choices = c(
            "predictor", "random", "fixed categorical",
            "covariate"
          ),
          options = list(
            title = "Choose component"
          )
        ),
        shinyWidgets::pickerInput(
          inputId = "edit_group",
          label = NULL,
          choices = c("a", "b", "c", "d"),
          options = list(
            title = "Group"
          )
        ),
      )
    ),
    body = shinydashboard::dashboardBody(
      shiny::fluidRow(
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Components",
          shiny::textOutput("output_component")
        ),
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Equation",
          shiny::textOutput("output_equation")
        ),
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Code",
          shiny::textOutput("output_code")
        )
      )
    )
  )
}
