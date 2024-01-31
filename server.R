# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

server <- function(input, output, session){

  test_test <- data.frame(x = 1:4, cool_dat = rnorm(4, 4, 0.6))
  name_tab <- shiny::reactiveValues(x = NULL)
  
  #update inputgroup with column headers(wrap in observe event after)
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "input_group",
    choices = c("observation", "interactions", colnames(test_test))
  )
  
  
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
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F,pageLength = 100)
        )
      )
  })


  
  #show or hide group name box if interaction/observation is not picked.
  shiny::observeEvent(input$input_group, {
    if(input$input_group == "observation"|
       input$input_group == "interactions"){
      shinyjs::hide("group_name")
    } 
     else {
      shinyjs::show("group_name")
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
    
}