####### shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024 - Updated 3 July 2025 #######

ui <- function() {
  ####### SETUP the Dashboard ######
  bs4Dash::dashboardPage(
    dark = NULL,
    help = NULL,
    fullscreen = TRUE,
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
      fresh::bs4dash_status(
        primary = "#5E81AC",
        danger = "#FAF9F6",
        success = "white"
      )
    ),

    ####### Header Code #########
    header = bs4Dash::dashboardHeader(
      shiny::tags$style(".main-header {max-height: 56px}"),
      title = bs4Dash::dashboardBrand(
        title = NULL,
        color = "primary",
        href = "https://github.com/squidgroup/shinySim",
        image = "github_icon.png"
      ),
      prompter::use_prompt(),
      status = "primary",
      fixed = TRUE,
      # Action button as the logo
      shiny::actionButton(
        inputId = "citeme",
        label = shiny::tags$img(src = "logo_1.png", height = "40px", width = "40px"),
        style = "position: absolute; right: 50px; background-color: #5E81AC; border-color: #5E81AC;",
      ) |>
        prompter::add_prompt(
          position = "left",
          message = "Click me!",
          type = "info",
          animate = TRUE,
          shadow = TRUE,
          arrow = TRUE
        )
    ),
    ####### SideBar Code #########
    sidebar = bs4Dash::dashboardSidebar(
      shiny::tags$style(shiny::HTML("
  .left-side, .main-sidebar {
    padding-top: 10px;
  }
")),
      fixed = "TRUE",
      status = "primary",
      minified = FALSE,
      elevation = 1,
      flat = TRUE,
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
      shiny::tags$head(shiny::tags$style(shiny::HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "
          .form-group {
            margin-bottom: 0 !important;
            padding: 0 !important;
          }
        "
          )
        )
      ),
      # tabs boxes
      bs4Dash::tabBox(
        id = "add_update_tab",
        title = NULL,
        selected = NULL,
        status = "primary",
        solidHeader = FALSE,
        type = "pills",
        collapsible = FALSE,
        width = 12,
        elevation = 1,
        ####### ADD Tab ########
        shiny::tabPanel(
          title = "Add",
          shiny::tags$div(id = "add_tab_container",
          # inputgroup
          shiny::splitLayout(
            shiny::tags$div(
              style = "text-align: center;",
              shinyWidgets::pickerInput(
                inputId = "input_group",
                label = shiny::tagList(
                  shiny::actionButton(
                    inputId = "click_group",
                    label = shiny::tags$span(
                      style = "white-space: nowrap;", "Select Group",
                      shiny::tags$span(style = "color: grey; margin-left: 0px;", "?")
                    ),
                    style = "border: none; background-color: transparent; padding: 0; font-weight: bold;"
                  )
                ),
                selected = NULL,
                choices = c("observation", "interactions"),
                options = list(
                  title = "Group"
                )
              )
            ),
            # hidden group name
            shiny::tags$div(
              style = "text-align: center;",
              shinyjs::hidden(
                shiny::textInput("input_component_name",
                  value = "",
                  label = shiny::tagList(
                    shiny::actionButton(
                      inputId = "click_component_name",
                      label = shiny::tags$span(
                        style = "white-space: nowrap;", "Component Name",
                        shiny::tags$span(style = "color: grey; margin-left: 0px;", "?")
                      ),
                      style = "border: none; background-color: transparent; padding: 0; font-weight: bold;"
                    )
                  )
                )
              )
            )
          ),
          # hidden input component
          shiny::splitLayout(
            shiny::tags$div(
              style = "text-align: center;",
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  inputId = "component_type",
                  label = shiny::tagList(
                    shiny::actionButton(
                      inputId = "click_component_type",
                      label = shiny::tags$span(
                        style = "white-space: nowrap;", "Component Type",
                        shiny::tags$span(style = "color: grey; margin-left: 0px;", "?")
                      ),
                      style = "border: none; background-color: transparent; padding: 0; font-weight: bold;"
                    )
                  ),
                  choices = c(
                    "predictor", "random", "fixed categorical",
                    "covariate"
                  ),
                  options = list(
                    title = "Type"
                  )
                )
              )
            ),
            # variable number
            shiny::tags$div(
              style = "text-align: center;",
              shinyjs::hidden(
                shiny::numericInput(
                  inputId = "input_variable_no",
                  label = shiny::tagList(
                    shiny::actionButton(
                      inputId = "click_variable_no",
                      label = shiny::tags$span(
                        style = "white-space: nowrap;", "Variable No.",
                        shiny::tags$span(style = "color: grey; margin-left: 0px;", "?")
                      ),
                      style = "border: none; background-color: transparent; padding: 0; font-weight: bold;"
                    )
                  ),
                  value = 1, min = 1, max = 10
                )
              )
            )
          ),
          # ADD tables
          shiny::fluidRow(
            # shiny::splitLayout(
            column(
              width = 6,
              shinyjs::hidden(
                shiny::tags$div(
                  id = "name_panel",
                  DT::DTOutput("name_table")
                )
              ),
              shinyjs::hidden(
                shiny::tags$div(
                  id = "interaction_panel",
                  shiny::tags$h2("Interaction variables", style = "text-align: left; color: black; font-size: small; font-weight: bold; margin-bottom: 0;
                margin-top: 0; padding: 0;"),
                  shinyWidgets::pickerInput(
                    inputId = "int_var1",
                    choices = c(""),
                    options = list(
                      title = "Variable 1"
                    )
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "int_var2",
                    choices = c(""),
                    options = list(
                      title = "Variable 2"
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              shinyjs::hidden(
                shiny::tags$div(
                  id = "beta_panel",
                  DT::DTOutput("beta_table")
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny::tags$div(
              id = "vcov_panel",
              DT::DTOutput("vcov_table")
            )
          ),
          shinyjs::hidden(
            shiny::tags$div(
              id = "mean_panel",
              DT::DTOutput("mean_table")
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

        ###### Update Tab ###########
        shiny::tabPanel(
          title = "Update",
          shiny::tags$div(id = "update_tab_container",
          # pickerinput
          shiny::splitLayout(
            shiny::tags$div(
              style = "text-align: center;",
              shinyWidgets::pickerInput(
                inputId = "choose_component",
                label = shiny::tags$span(style = "color: black;", "Select Component"),
                selected = NULL,
                choices = c("intercept", "residual"),
                options = list(
                  title = "Component"
                )
              )
            ),

            # hidden input component
            shinyjs::hidden(
              shiny::tags$div(
                style = "text-align: center; font-weight: bold;",
                id = "component_type_edit_div",
                "Component Type"
              ),
              shiny::tags$div(
                id = "component_type_edit_div_print",
                style = "text-align: center;",
                shiny::br(),
                shiny::uiOutput("component_type_edit_print")
              )
            )
          ),

          # tables#
          # name
          shiny::splitLayout(
            shinyjs::hidden(
              shiny::tags$div(
                style = "text-align: center;",
                id = "name_panel_edit",
                DT::DTOutput("name_table_edit")
              )
            ),
            # mean
            shinyjs::hidden(
              shiny::tags$div(
                style = "text-align: center;",
                id = "beta_panel_edit",
                DT::DTOutput("beta_table_edit")
              )
            )
          ),
          shinyjs::hidden(
            shiny::tags$div(
              style = "text-align: center;",
              id = "vcov_panel_edit",
              DT::DTOutput("vcov_table_edit")
            )
          ),
          shinyjs::hidden(
            shiny::tags$div(
              style = "text-align: center;",
              id = "mean_panel_edit",
              DT::DTOutput("mean_table_edit")
            )
          ),
          shiny::tags$div(
            style = "text-align: center;",
            shinyjs::hidden(
              shiny::numericInput(
                inputId = "intercept_panel",
                label = shiny::tags$span(style = "color: black;", "Intercept"),
                value = 0
              )
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
          shiny::splitLayout(
            shiny::tags$div(
              style = "display: flex; justify-content: center;",
              shiny::actionButton(
                "update_parameters",
                label = "Update",
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::tags$div(
              style = "display: flex; justify-content: center;",
              shiny::actionButton(
                "delete_parameters",
                label = "Delete",
                style = "color: #ffffff; background-color: #ff0000; border-color: #ff0000"
              )
            )
          )
        )
      )
    )
    ),
    ####### Body Code #########
    body = bs4Dash::dashboardBody(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "
          .form-group {
            margin-bottom: 0 !important;
          }
        "
          )
        )
      ),
      shiny::fluidRow(
        bs4Dash::column(
          width = 8,
          ## component output
          bs4Dash::box(
            width = NULL,
            status = "primary",
            background = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            elevation = 3,
            title = "Simulation Components",
            shiny::tags$div(
              style = "text-align: center;",
              shiny::uiOutput("output_component")
            )
          ),
          ## equation output
          bs4Dash::box(
            width = NULL,
            status = "primary",
            background = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            elevation = 3,
            title = "Simulation Equation",
            shiny::uiOutput("output_equation")
          ),
          ## code output
          bs4Dash::box(
            status = "primary",
            background = "danger",
            width = NULL,
            collapsible = FALSE,
            elevation = 3,
            solidHeader = TRUE,
            title = "Variance Decomposition",
            shiny::uiOutput("output_variance"),
            br(),
            fluidRow(
              column(
                width = 12,
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  "Contribution of different hierarchical levels:",
                  "Contribution of different predictors:"
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  shiny::tableOutput("output_variance_mid_tab"),
                  shiny::tableOutput("output_variance_right_tab")
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  shiny::plotOutput("output_variance_mid_plot", height = "200px", width = "50%"),
                  shiny::plotOutput("output_variance_right_plot", height = "200px", width = "50%")
                )
              )
            )
          )
        ),
        bs4Dash::column(
          width = 4,
          # variance output
          bs4Dash::box(
            elevation = 3,
            width = NULL,
            status = "primary",
            background = "danger",
            collapsible = FALSE,
            solidHeader = TRUE,
            title = "Simulation Code",
            shiny::uiOutput("output_code")
          )
        )
      )
    )
  )
}
