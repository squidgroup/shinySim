# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

ui <- function() {
  shinydashboard::dashboardPage(
    #packages we need
    shinyjs::useShinyjs(),
    
    #headercode
    header = shinydashboard::dashboardHeader(
      title = "shinySim",
      shinydashboardPlus::userOutput("user")
    ),
    
    #sidebarcode
    sidebar = shinydashboard::dashboardSidebar(
      width = 300,
      shiny::br(),
      
      ##input sturcture box
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Data Structure",
        #textinput
        shiny::textInput("input_structure", label = NULL)
      ),
      
      ##input component box
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Add Component",
        #pickerinput
        shinyWidgets::pickerInput(
          inputId = "input_group",
          label = NULL,
          selected = NULL,
          choices = c("observation", "interactions", "c"),
          options = list(
            title = "Group"
          )
        ),
        #hidden group name
        shinyjs::hidden(
          shiny::div(
            id = "component_name",
            shiny::textInput("input_component_name",
                             value = "",
              label = shiny::tags$span(style = "color: grey;", "Component Name (optional)")
            )
          )
        ),
        #variable number
        shiny::numericInput(
          inputId = "input_variable_no",
          label = shiny::tags$span(style = "color: black;", "Number of Variables"),
          value = 1, min = 1, max = 10
        ),
        #hidden input component
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
        ),
        #tables#
          DT::DTOutput("name_table"),
          DT::DTOutput("mean_table"),
          DT::DTOutput("beta_table"),
          DT::DTOutput("vcov_table"),
        #covcorr radio
          shinyWidgets::radioGroupButtons(
            inputId = "input_covcorr",
            label = NULL,
            choices = c("Corr", 
                        "Cov"),
            direction = "horizontal",
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-check-square", 
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-square-o", 
                          style = "color: steelblue"))
          ),
        shiny::actionButton(inputId = "add_to_parameters", "Add")
      ),

      # depending on each component different things come up.
      # Hidden boxes - four things. little tables.
      # names - always comes up, depends on numerinput
      # names, mean and beta, number of elements equal to input variable number.
      # 1xn
      # vcov, little matrix (n by n).
      #n = variables
      # radio button with covariance or correlation.

    ##edit compment box
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Edit Component",
        #pickerinput for component
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
        #pickerinput to select group
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
    #dashbaord main
    body = shinydashboard::dashboardBody(
      shiny::fluidRow(
        ##component output
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Components",
          shiny::uiOutput("output_component")
        ),
        ##equation output
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Equation",
          shiny::uiOutput("output_equation")
        ),
        ##code output
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = "Simulation Code",
          shiny::uiOutput("output_code")
        )
      )
    )
  )
}
