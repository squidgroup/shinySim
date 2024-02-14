# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

ui <- function() {
  shinydashboard::dashboardPage(
    #packages we need
    shinyjs::useShinyjs(),
    
    #headercode
    header = shinydashboardPlus::dashboardHeader(
      title = "shinySim",
      titleWidth = 300,
      shinydashboardPlus::userOutput("user"),
      controlbarIcon = NULL
    ),
    
    #sidebarcode
    sidebar = shinydashboard::dashboardSidebar(
      width = 300,
      shiny::br(),
      
    tabsetPanel(
    type = "tabs",
    tabPanel("Add", 

      ##input sturcture box
      shinydashboard::box(
       width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Add Component",
                
        shinyWidgets::pickerInput(
          inputId = "input_group",
          label = shiny::tags$span(style = "color: black;", "Select a group"),
          selected = NULL,
          choices = c("observation", "interactions"),
          options = list(
            title = "Group"
          )
        ),
        #hidden group name
        shinyjs::hidden(
          shiny::textInput("input_component_name",
            value = "",
            label = shiny::tags$span(style = "color: grey;", "Component Name (optional)")
          )
        ),
        
        # hidden input component
        shinyjs::hidden(
        shinyWidgets::pickerInput(
          inputId = "component_type",
          label = NULL,
          choices = c(
            "predictor", "random", "fixed categorical",
            "covariate"
          ),
          options = list(
            title = "Component type"
          )
        )
          
        ),

        #variable number
        shinyjs::hidden(
          shiny::numericInput(
            inputId = "input_variable_no",
            label = shiny::tags$span(style = "color: black;", "Number of Variables"),
            value = 1, min = 1, max = 10
        )),
        
      #   #code to remove numeric inputs
        shiny::tags$head(
          shiny::tags$style(shiny::HTML("
      .dataTables_wrapper input[type='number']::-webkit-inner-spin-button,
      .dataTables_wrapper input[type='number']::-webkit-outer-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      "))),
    
        #ADD tables
        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="name_panel",
            h3("Name", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
            DT::DTOutput("name_table")
        )),

        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="mean_panel",
           h3("Mean", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
           DT::DTOutput("mean_table")
        )),

        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="vcov_panel",
            h3("VCov", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
            DT::DTOutput("vcov_table")
        )),
    
        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="beta_panel",
            h3("Beta", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
            DT::DTOutput("beta_table")
        )),

        shiny::tags$div(
          style = "display: flex; justify-content: center;",
          shiny::actionButton(
          "add_component",
          label = "Add",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )     
      )

      ),
    tabPanel("Update", 
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Modify Component",
        #pickerinput
        shinyWidgets::pickerInput(
          inputId = "choose_component",
          label = shiny::tags$span(style = "color: black;", "Select Component"),
          selected = NULL,
          choices = c("intercept","residual"),
          options = list(
            title = "Component"
          )
        ),
        
        #hidden input component
        shinyjs::hidden(

        shinyWidgets::pickerInput(
          inputId = "component_type_edit",
          label = NULL,
          choices = c(
            "predictor", "random", "fixed categorical",
            "covariate"
          ),
          options = list(
            title = "Component type"
          )
        )
        ),

        #variable number
        shinyjs::hidden(
          shiny::numericInput(
            inputId = "input_variable_no_edit",
            label = shiny::tags$span(style = "color: black;", "Number of Variables"),
            value = 1, min = 1, max = 10
        )),
        
        #code to remove numeric inputs
        shiny::tags$head(
          shiny::tags$style(shiny::HTML("
      .dataTables_wrapper input[type='number']::-webkit-inner-spin-button,
      .dataTables_wrapper input[type='number']::-webkit-outer-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      "))),
    
        #tables#
        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="name_panel_edit",
            h3("Name", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
            # DT::DTOutput("name_table")
        )),

        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="mean_panel_edit",
           h3("Mean", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
           # DT::DTOutput("mean_table")
        )),

        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="vcov_panel_edit",
            h3("VCov", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
            # DT::DTOutput("vcov_table")
        )),
    
        shinyjs::hidden(
          shiny::wellPanel(style = "background: white",id="beta_panel_edit",
            h3("Beta", style = "text-align: left; color: black; font-size: small; font-weight: bold;margin-bottom: 0;
              margin-top: 0;"),
            # DT::DTOutput("beta_table")
        )),
    
        shinyjs::hidden(
          shiny::numericInput(
            inputId = "intercept_panel",
            label = shiny::tags$span(style = "color: black;", "Intercept"),
            value = 0
        )),
            #covcorr radio
        #   shinyWidgets::radioGroupButtons(
        #     inputId = "input_covcorr",
        #     label = NULL,
        #     choices = c("Corr", 
        #                 "Cov"),
        #     direction = "horizontal",
        #     individual = TRUE,
        #     justified = TRUE,
        #     checkIcon = list(
        #       yes = tags$i(class = "fa fa-check-square", 
        #                    style = "color: steelblue"),
        #       no = tags$i(class = "fa fa-square-o", 
        #                   style = "color: steelblue"))
        #   ),
        # br(),
        shiny::tags$div(
          style = "display: flex; justify-content: center;",
          shiny::actionButton(
          "update_parameters",
          label = "Update",
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
        )
    ))


      
      ##update component box
      
      ),

  
    #dashbaord main
    body = shinydashboard::dashboardBody(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(width = 8,
        ##component output
        shinydashboard::box(
          width = NULL, status = "primary", solidHeader = TRUE,
          title = "Simulation Components",
          shiny::uiOutput("output_component")
        ),
        ##equation output
        shinydashboard::box(
          width = NULL, status = "primary", solidHeader = TRUE,
          title = "Simulation Equation",
          shiny::uiOutput("output_equation")
        ),
        ##code output
        shinydashboard::box(
          status = "primary", 
          solidHeader = TRUE,
          width = NULL,
          title = "Simulation Code",
          shiny::uiOutput("output_code")
        )
        ),
        shiny::column(width = 4,
        #variance output
        shinydashboard::box(
          width = NULL,
          status = "primary", solidHeader = TRUE,
          title = "Variance",
          shiny::uiOutput("output_variance")
        )
        )
        )
      )
    )
}
