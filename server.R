# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

server <- function(input, output, session){
  
  #named_list
  name_list <- shiny::reactiveValues(x = NULL)
  #table data
  name_tab <- shiny::reactiveValues(x = data.frame(Name = NA))
  beta_tab <- shiny::reactiveValues(x = data.frame(Beta = NA))
  mean_tab <- shiny::reactiveValues(x = data.frame(Mean = NA))
  vcov_tab <- shiny::reactiveValues(x = data.frame(Vcov = NA))
  #parameter list
  param_list <- shiny::reactiveValues(residual = list(vcov = 1), intercept = 0)
  
  #update inputgroup with column headers(wrap in observe event after)
  shiny::observeEvent(input$input_structure, {
  
    if(input$input_structure !=""){ 
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "input_group",
    choices = c(input$input_structure, "observation", "interactions")
    )
    } else(shinyWidgets::updatePickerInput(
      session = session,
      inputId = "input_group",
      choices = c("observation", "interactions")
    ))
  })
  
  #create tables based on input
  shiny::observeEvent(input$input_variable_no, {
    
    observe({
      num_rows <- input$input_variable_no
      new_rows <- rep("NA", num_rows)
      update_df <- data.frame(Name = new_rows)
      name_tab$x <- update_df
    })
    
    observe({
      num_rows <- input$input_variable_no
      new_rows <- rep("NA", num_rows)
      update_df <- data.frame(Beta = new_rows)
      beta_tab$x <- update_df
    })
    
    observe({
      num_rows <- input$input_variable_no
      new_rows <- rep("NA", num_rows)
      update_df <- data.frame(Mean = new_rows)
      mean_tab$x <- update_df
    })
    
    #vcov_table
    observe({
      num_rows <- input$input_variable_no
      num_cols <- input$input_variable_no
      update_df <- data.frame(matrix(0, ncol = num_cols, nrow = num_rows))
      colnames(update_df) <- 1:num_rows
      vcov_tab$x <- update_df
    })
    
      output$name_table <- DT::renderDT(
        DT::datatable(
          name_tab$x,
          selection = 'none',
          editable = list(target = "cell"),
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )

      output$mean_table <- DT::renderDT(
        DT::datatable(
          mean_tab$x,
          editable = list(target = "cell"),
          selection = 'none',
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )
      

      output$beta_table <- DT::renderDT(
        DT::datatable(
          beta_tab$x,
          editable = list(target = "cell"),
          selection = 'none',
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        )
      )

      output$vcov_table <- DT::renderDT(
        DT::datatable(
          vcov_tab$x,
          selection = 'none',
          editable = list(target = "cell"),
          class = 'cell-border stripe',
          options = list(scrollX = TRUE,lengthChange = TRUE, dom = "t", ordering = F,pageLength = 100)
        )
      )

  })
  #create proxies to edit data
  proxy_vcov <- DT::dataTableProxy("vcov_table")
  proxy_name <- DT::dataTableProxy("name_table")
  proxy_beta <- DT::dataTableProxy("beta_table")
  proxy_mean <- DT::dataTableProxy("mean_table")
  
  
  #record the data edit
  shiny::observeEvent(input$name_table_cell_edit, {
    info <- input$name_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    name_tab$x[i, j] <<- DT::coerceValue(v, name_tab$x[i, j])
    DT::replaceData(proxy_name, name_tab$x, resetPaging = FALSE)
   str(name_tab$x)
   print(name_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$vcov_table_cell_edit, {
    info <- input$vcov_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    vcov_tab$x[i, j] <<- DT::coerceValue(v, vcov_tab$x[i, j])
    DT::replaceData(proxy_vcov, vcov_tab$x, resetPaging = FALSE)
    str(vcov_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$beta_table_cell_edit, {
    info <- input$beta_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    beta_tab$x[i, j] <<- DT::coerceValue(v, beta_tab$x[i, j])
    DT::replaceData(proxy_beta, beta_tab$x,resetPaging = FALSE)
    str(beta_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$mean_table_cell_edit, {
    info <- input$mean_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    mean_tab$x[i, j] <<- DT::coerceValue(v, mean_tab$x[i, j])
    DT::replaceData(proxy_mean, mean_tab$x,resetPaging = FALSE)
    str(mean_tab$x)
  })

  #add button adds to list
  shiny::observeEvent(input$add_to_parameters, {

    if(nchar(input$input_component_name) == 0){
      name_list$x <- input$input_group
    } else (name_list$x <- input$input_component_name)
    
    param_list[[name_list$x]] <- list(group = input$input_group,
                                      name = name_tab$x,
                                      beta = beta_tab$x,
                                      mean = mean_tab$x,
                                      vcov = vcov_tab$x)
    
    print(param_list[[name_list$x]])
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
            href = "https://github.com/squidgroup/shinySim",
            icon = icon("github")
          )
        )
      )
    )
          })
  
  
  output$output_equation <- renderUI({
    
    shiny::withMathJax("$$\\color{#000000}{\\beta_0} + \\color{#009E73}{w_{j}} + \\color{#F0E442}{\\beta_{v} v_{j}} + \\color{#56B4E9}{\\beta_{x,1} x_{1,i}} + \\color{#56B4E9}{\\beta_{x,2} x_{2,i}} + \\color{#E69F00}{\\epsilon_i}$$")
    
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