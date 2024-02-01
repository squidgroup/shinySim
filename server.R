# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

server <- function(input, output, session){
  name_tab <- shiny::reactiveValues(x = NULL)
  name_list <- shiny::reactiveValues(x = NULL)
  
  param_list <<- shiny::reactiveValues(
  residual = list(vcov = 1),
  intercept = 0,
  )
  
  #update inputgroup with column headers(wrap in observe event after)
  shiny::observeEvent(input$input_structure, {

  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "input_group",
    choices = c(input$input_structure, "observation", "interactions")
    )
  })
  
  #create tables based on input
  shiny::observeEvent(input$input_variable_no, {
    
    
    #name table
  name_table <- reactive({
    num_rows <- input$input_variable_no
    df <- data.frame(matrix(ncol = 1, nrow = num_rows))
    colnames(df) <- c("Name")
    df
  })
      
      output$name_table <- DT::renderDT(
        DT::datatable(
          name_table(),
          rownames= FALSE,
          editable = list(target = "cell"),
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )
      
      #mean_table
      mean_table <- reactive({
        num_rows <- input$input_variable_no
        df <- data.frame(matrix(ncol = 1, nrow = num_rows))
        colnames(df) <- c("Mean")
        df
      })
      
      output$mean_table <- DT::renderDT(
        DT::datatable(
          mean_table(),
          editable = list(target = "cell"),
          rownames= FALSE,
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )
      
      #beta_table
      beta_table <- reactive({
        num_rows <- input$input_variable_no
        df <- data.frame(matrix(ncol = 1, nrow = num_rows))
        colnames(df) <- c("beta")
        df
      })
      
      output$beta_table <- DT::renderDT(
        DT::datatable(
          beta_table(),
          editable = list(target = "cell"),
          rownames= FALSE,
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )
      
      #vcov_table
      vcov_table <- reactive({
        num_rows <- input$input_variable_no
        num_cols <- input$input_variable_no
        df <- data.frame(matrix(ncol = num_cols, nrow = num_rows))
        colnames(df) <- 1:num_rows
        df
      })
      
      output$vcov_table <- DT::renderDT(
        DT::datatable(
          vcov_table(),
          rownames= FALSE,
          editable = list(target = "cell"),
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F,pageLength = 100)
        )
      )
  })

  #add button adds to list
  shiny::observeEvent(input$add_to_parameters, {

    if(nchar(input$input_component_name) == 0){
      name_list$x <- input$input_group
    } else (name_list$x <- input$input_component_name)
    
    param_list[[name_list$x]] <- list(group = input$input_group)
                                  #names = #nametable
                                 #mean = #meantable,
                                 #beta = #beta table,
                                #vcov = #vcovtable
    
    print(param_list)
   })

  
  #show or hide group name box if interaction/observation is not picked.
  shiny::observeEvent(input$input_group, {
    if(input$input_group == "observation"|
       input$input_group == "interactions"){
      shinyjs::hide("component_name")
    } 
     else {
      shinyjs::show("component_name")
     }
  })
  
  shiny::observeEvent(input$input_group, {
    if(input$input_group == "observation"|
       input$input_group == "interactions"){
      shinyjs::hide("component_type")
    } 
    else {
      shinyjs::show("component_type")
    }
  })
  
  #user name for popup
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      image = "squidSim_logo.png", 
      name = "shinySim",
      subtitle = "by Ed Ivimey-Cook & Joel Pick", 
      shiny::fluidRow(
        shinydashboardPlus::dashboardUserItem(
          width = 12,
          shinydashboardPlus::socialButton(
            href = "https://github.com",
            icon = icon("github")
          )
        )
      )
    )
          })
  

  output$output_equation <- renderUI({
  
    withMathJax("$$\\color{#000000}{\\beta_0} + \\color{#009E73}{w_{j}} + \\color{#F0E442}{\\beta_{v} v_{j}} + \\color{#56B4E9}{\\beta_{x,1} x_{1,i}} + \\color{#56B4E9}{\\beta_{x,2} x_{2,i}} + \\color{#E69F00}{\\epsilon_i}$$")

  })

  output$output_component <- renderText({
   paste(
    "<span style=\"color:#000000\">intercept</span>",
    "<span style=\"color:#009E73\">individual_random</span>",
    "<span style=\"color:#F0E442\">individual_predictors</span>",
    "<span style=\"color:#56B4E9\">observation</span>",
    "<span style=\"color:#E69F00\">residual</span>",
     sep=" + ")
})
}