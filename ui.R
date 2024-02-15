####### shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024########

ui <- function() {
  ####### SETUP the Dashboard######
  bs4Dash::dashboardPage(
    help = NULL,
    # theme settings
    freshTheme = fresh::create_theme(
      fresh::bs4dash_vars(
        navbar_light_color = "#086A87",
        navbar_light_active_color = "#086A87",
        navbar_light_hover_color = "#FFF"
      ),
      fresh::bs4dash_layout(
        main_bg = "white",
        sidebar_width = "350px"
      ),
      fresh::bs4dash_sidebar_light(
        bg = "white",
        hover_bg = "lightblue",
        submenu_bg = "lightblue",
      ),
      fresh::bs4dash_sidebar_dark(
        bg = "black",
        submenu_bg = "white",
        submenu_color = "white",
        active_color = "black"
      ),
      fresh::bs4dash_status(
        primary = "#5E81AC",
        danger = "#FAF9F6",
        success = "white"
      )
    ),
    
    ####### Header Code#########
    header = bs4Dash::dashboardHeader(
      status = "primary",
      fixed = TRUE,
      # add some shinysim details
      title = bs4Dash::dashboardBrand(
        title = "shinySim",
        color = "primary",
        href = "https://github.com/squidgroup/shinySim",
        image = "squidSim_logo.png"
      )
    ),
    
    ####### SideBar Code#########
    sidebar = bs4Dash::dashboardSidebar(
      
      # packages and functions
      shinyjs::useShinyjs(),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML("
      .dataTables_wrapper input[type='number']::-webkit-inner-spin-button,
      .dataTables_wrapper input[type='number']::-webkit-outer-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      ")
        )
      ),

      shiny::br(),
      minified = FALSE,
      # tabs boxes
      bs4Dash::tabBox(
        id = "add_update_tab",
        title = NULL,
        selected = "Add",
        status = "primary",
        solidHeader = FALSE,
        type = "pills",
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        elevation = 3,
        ####### ADD Tab ########
        shiny::tabPanel(
          title = "Add",
          bs4Dash::box(
            width = NULL,
            closable = FALSE,
            status = "primary",
            background = "success",
            solidHeader = TRUE,
            elevation = 1,
            collapsible = FALSE,
            title = "Add Component",
            # inputgroup
            shinyWidgets::pickerInput(
              inputId = "input_group",
              label = shiny::tags$span(style = "color: black;", "Select a group"),
              selected = NULL,
              choices = c("observation", "interactions"),
              options = list(
                title = "Group"
              )
            ),
            # hidden group name
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
                label = shiny::tags$span(style = "color: black;", "Component type"),
                choices = c(
                  "predictor", "random", "fixed categorical",
                  "covariate"
                ),
                options = list(
                  title = "Component type"
                )
              )
            ),
            # variable number
            shinyjs::hidden(
              shiny::numericInput(
                inputId = "input_variable_no",
                label = shiny::tags$span(style = "color: black;", "Number of Variables"),
                value = 1, min = 1, max = 10
              )
            ),
            # ADD tables
            shinyjs::hidden(
              shiny::tags$div(id = "name_panel",
              bs4Dash::box(
                width = 12, 
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                closable = FALSE,
                elevation = 1,
                title = shiny::tags$h2("Name", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("name_table")
              )
            )
            ),
            shinyjs::hidden(
              shiny::tags$div(id = "mean_panel",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("Mean", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("mean_table")
                              )
              )
            ),
            shinyjs::hidden(
              shiny::tags$div(id = "vcov_panel",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("VCov", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("vcov_table")
                              )
              )
            ),
            shinyjs::hidden(
              shiny::tags$div(id = "beta_panel",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("Beta", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("beta_table")
                              )
              )
            ),
            shiny::tags$div(
              style = "display: flex; justify-content: center;",
              shiny::actionButton(
                "add_component",
                label = "Add",
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
        ),
        
        ####### Update Tab###########
        shiny::tabPanel(
          title = "Update",
            bs4Dash::box(
              width = NULL,
              closable = FALSE,
              status = "primary",
              background = "success",
              solidHeader = TRUE,
              elevation = 1,
              collapsible = FALSE,
              title = "Modify Component",
            # pickerinput
            shinyWidgets::pickerInput(
              inputId = "choose_component",
              label = shiny::tags$span(style = "color: black;", "Select Component"),
              selected = NULL,
              choices = c("intercept", "residual"),
              options = list(
                title = "Component"
              )
            ),
            
            # hidden input component
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
            
            # variable number
            shinyjs::hidden(
              shiny::numericInput(
                inputId = "input_variable_no_edit",
                label = shiny::tags$span(style = "color: black;", "Number of Variables"),
                value = 1, min = 1, max = 10
              )
            ),
            
            # tables#
            #name
            shinyjs::hidden(
              shiny::tags$div(id = "name_panel_edit",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("Name", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("name_table_edit")
                              )
              )
            ),
            #mean
            shinyjs::hidden(
              shiny::tags$div(id = "mean_panel_edit",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("Mean", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("mean_table_edit")
                              )
              )
            ),
            shinyjs::hidden(
              shiny::tags$div(id = "vcov_panel_edit",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("VCov", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("vcov_table_edit")
                              )
              )
            ),
            shinyjs::hidden(
              shiny::tags$div(id = "beta_panel_edit",
                              bs4Dash::box(
                                width = 12, 
                                status = "primary",
                                solidHeader = FALSE,
                                collapsible = FALSE,
                                closable = FALSE,
                                elevation = 1,
                                title = shiny::tags$h2("Beta", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
              margin-top: 0;"),
              DT::DTOutput("beta_table_edit")
                              )
              )
            ),
            shinyjs::hidden(
              shiny::numericInput(
                inputId = "intercept_panel",
                label = shiny::tags$span(style = "color: black;", "Intercept"),
                value = 0
              )
            ),
            # covcorr radio
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
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
        )
      )
    ),
    ####### Body Code#########
    body = bs4Dash::dashboardBody(
      shiny::br(),
      shiny::fluidRow(
        bs4Dash::column(
          width = 8,
          ## component output
          bs4Dash::box(
            width = NULL,
            status = "primary",
            background = "danger",
            solidHeader = TRUE,
            closable = FALSE,
            collapsible = FALSE,
            elevation = 3,
            title = "Simulation Components",
            shiny::uiOutput("output_component")
          ),
          ## equation output
          bs4Dash::box(
            width = NULL,
            closable = FALSE,
            status = "primary",
            background = "danger",
            solidHeader = TRUE,
            elevation = 3,
            collapsible = FALSE,
            title = "Simulation Equation",
            shiny::uiOutput("output_equation")
          ),
          ## code output
          bs4Dash::box(
            status = "primary",
            solidHeader = TRUE,
            closable = FALSE,
            background = "danger",
            width = NULL,
            elevation = 3,
            collapsible = FALSE,
            title = "Simulation Code",
            shiny::uiOutput("output_code")
          )
        ),
        bs4Dash::column(
          width = 4,
          # variance output
          bs4Dash::box(
            elevation = 3,
            width = NULL,
            closable = FALSE,
            status = "primary",
            background = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            title = "Variance",
            shiny::uiOutput("output_variance")
          )
        )
      )
    )
  )
}
