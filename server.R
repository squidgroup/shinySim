# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

manyToggle <- function(show=NULL,hide=NULL){
  for(i in 1:length(show)) shinyjs::show(show[i])
  for(j in 1:length(hide)) shinyjs::hide(hide[j])
}

server <- function(input, output, session){

  ## list of added components as well as interactions
  component_list <- shiny::reactiveValues(
    x = data.frame(component=c("intercept","residual"),group=c("intercept","residual")))
  #named_list
  name_list <- shiny::reactiveValues(x = NULL)
  #table data
  name_tab <- shiny::reactiveValues(x = data.frame(Name = NA))
  beta_tab <- shiny::reactiveValues(x = data.frame(Beta = NA))
  mean_tab <- shiny::reactiveValues(x = data.frame(Mean = NA))
  vcov_tab <- shiny::reactiveValues(x = data.frame(Vcov = NA))
  
  residual_start = list(vcov = matrix(1), beta=matrix(1), mean=0,group="residual",names="residual", fixed=FALSE, covariate=FALSE)

  #parameter list
  param_list <- shiny::reactiveValues(intercept = 0,residual=residual_start)
  # list containing components, equation and  code for output
  output_list <- shiny::reactiveValues(
    x = make_equation(list(intercept = 0,residual = residual_start))
    )  
  var_list <- shiny::reactiveValues(
    x = simulated_variance(list(intercept = 0,residual = residual_start),data.struc)
    )

  # print(reactiveValuesToList(param_list))
  
  #importing datas structure
  shiny::observe({
     if(nrow(data.struc) > 0 ){
       name_list$x <- get(x = "data.struc", envir = globalenv())
       # print(colnames(name_list$x))
    } else
    name_list$x <- NULL
  })
  
  #update inputgroup with column headers(wrap in observe event after)
  shiny::observe({
    
    if(nrow(data.struc) > 0 ){ 
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "input_group",
        choices = c(colnames(name_list$x), "observation", "interactions")
      )
    } else(shinyWidgets::updatePickerInput(
      session = session,
      inputId = "input_group",
      choices = c("observation", "interactions")
    ))
  })

  
  

#####
# ---  ADDING IN COMPONENT
####


shiny::observeEvent(input$input_group, {

   shiny::updateNumericInput(session = session, 
                                  inputId = "input_variable_no", 
                                  value = 1)
    #show or hide group name box if interaction/observation is not picked.
      # print(input$input_group)

    if(input$input_group %in% c("","observation","interactions")){
      manyToggle(hide=c("component_type","input_component_name"))
    }else{
      manyToggle(
        show=c("component_type","input_component_name"),
        hide=c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel")
      )
    }

    if(input$input_group %in% c("")){
      manyToggle( 
        hide=c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel")
      )
    }

    if(input$input_group == c("observation")){
      manyToggle(
        show=c("input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel")
      )
    }
    
    if(input$input_group == c("interactions")){
      manyToggle(
        show=c("input_variable_no", "name_panel", "beta_panel"),
        hide=c( "mean_panel", "vcov_panel")
      )
    }
    
  
  })
  
  shiny::observeEvent(input$component_type, {
  
   shiny::updateNumericInput(session = session, 
                            inputId = "input_variable_no", 
                            value = 1)

    if(input$component_type==c("predictor")){ 
      manyToggle(
        show=c("input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel")
      )
    }
  
    if(input$component_type==c("random")){
      manyToggle(
        show=c("input_variable_no", "name_panel", "vcov_panel", "beta_panel"),
        hide="mean_panel"
      )

    }

    if(input$component_type==c("fixed categorical")){
      
      manyToggle(
        show=c("name_panel", "beta_panel"),
        hide=c("input_variable_no","mean_panel", "vcov_panel")
      )
      
      num_level<-length(unique(name_list$x[[input$input_group]]))
      if(num_level>1){
        shiny::updateNumericInput(session = session, 
                                  inputId = "input_variable_no", 
                                  value = num_level)
      }
      
    }
    
    if(input$component_type==c("covariate")){
      manyToggle(
        show=c("name_panel", "beta_panel"),
        hide=c("input_variable_no","mean_panel", "vcov_panel")
      )
    }
    
    
  })





#################

#table stuff


  #create tables based on input
  shiny::observeEvent(input$input_variable_no, {
      num_rows <- input$input_variable_no
  
      name_tab$x <- data.frame(Name = if(input$component_type=="fixed categorical"){
        as.character(unique(name_list$x[[input$input_group]]))
      }else{
        rep("", num_rows)
      })
      beta_tab$x <- data.frame(Beta = rep(1, num_rows))#})
     # observe({ 
      mean_tab$x <- data.frame(Mean = rep(0, num_rows))#})
     # observe({ 
      vcov_update <- data.frame(diag(num_rows)) 
      colnames(vcov_update) <- 1:num_rows#})
      vcov_tab$x <- vcov_update
    
    js <- "table.on('click', 'td', function() { 
    $(this).dblclick();
  });"
    
    
      output$name_table <- DT::renderDT(
        DT::datatable(
          name_tab$x,
          selection = 'none',
          rownames = FALSE,
          colnames = NULL,
          callback = DT::JS(js),
          editable = list(target = "cell"),
          class = list(stripe = FALSE),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100,
                         rowCallback = DT::JS(
                           'function(row, data) {',
                           '$("td", row).css("height", "20px");', # Set row height
                           '}'
                         )),
        ) |> DT::formatStyle(1,`text-align` = 'left') |>
          DT::formatStyle(names(name_tab$x), lineHeight = '30px')
      )

      output$mean_table <- DT::renderDT(
        DT::datatable(
          mean_tab$x,
          rownames = FALSE,
          colnames = NULL,
          callback = DT::JS(js),
          editable = list(target = "cell"),
          selection = 'none',
          class = list(stripe = FALSE),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        ) |> DT::formatStyle(1,`text-align` = 'left')
      )
      

      output$beta_table <- DT::renderDT(
        DT::datatable(
          beta_tab$x,
          editable = list(target = "cell"),
          selection = 'none',
          rownames = FALSE,
          colnames = NULL,
          callback = DT::JS(js),
          class = list(stripe = FALSE),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
          ) |> DT::formatStyle(1,`text-align` = 'left')
      )

      output$vcov_table <- DT::renderDT(
        DT::datatable(
          vcov_tab$x,
          selection = 'none',
          rownames = FALSE,
          colnames = NULL,
          callback = DT::JS(js),
          editable = list(target = "cell"),
          class = list(stripe = FALSE),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F,pageLength = 100)
        ) |> DT::formatStyle(1:nrow(vcov_tab$x),`text-align` = 'left')
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
    j <- info$col+1
    v <- info$value
    
    name_tab$x[i, j] <<- DT::coerceValue(v, name_tab$x[i, j])
    DT::replaceData(proxy_name, name_tab$x, resetPaging = FALSE)
   # print(name_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$vcov_table_cell_edit, {
    info <- input$vcov_table_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value
    
    vcov_tab$x[i, j] <<- DT::coerceValue(v, vcov_tab$x[i, j])
    DT::replaceData(proxy_vcov, vcov_tab$x, resetPaging = FALSE)
    str(vcov_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$beta_table_cell_edit, {
    info <- input$beta_table_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value
    
    beta_tab$x[i, j] <<- DT::coerceValue(v, beta_tab$x[i, j])
    DT::replaceData(proxy_beta, beta_tab$x,resetPaging = FALSE)
    str(beta_tab$x)
  })
  
  #record the data edit
  shiny::observeEvent(input$mean_table_cell_edit, {
    info <- input$mean_table_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value
    
    mean_tab$x[i, j] <<- DT::coerceValue(v, mean_tab$x[i, j])
    DT::replaceData(proxy_mean, mean_tab$x,resetPaging = FALSE)
    str(mean_tab$x)
  })
  



  ### what happens when add component button is pressed
  shiny::observeEvent(input$add_component, {
    
      comp_name <- if(nchar(input$input_component_name) == 0){
          input$input_group
        }else{
          input$input_component_name
        } 
      comp_group <- input$input_group


    if(input$input_group == ""){
      shinyalert::shinyalert(title = "Please select a group", type = "error")
    }else if(comp_name %in% component_list$x$component){
      shinyalert::shinyalert(title = "Component already added", type = "error")
    }else{
    
    
      component_list$x<- data.frame(
        component=c(component_list$x$component,comp_name),
        group=c(component_list$x$group,comp_group))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "choose_component",
        choices = component_list$x$component
      )
    
    # adds the component and group to the component list
 
      
      #make squidSim parameter list 
      param_list[[comp_name]] <- list(
        group = comp_group,
        beta = unname(as.matrix(beta_tab$x)),
        mean = as.numeric(as.matrix(mean_tab$x)),
        vcov = unname(as.matrix(vcov_tab$x))
      )
      
      if(comp_group != "interactions"){
          param_list[[comp_name]]$fixed <- input$component_type == "fixed categorical"
          param_list[[comp_name]]$covariate <- input$component_type == "covariate"
      }

      ## add in names if they are entered
      param_list[[comp_name]]$names <- 
        if(!all(nchar(as.matrix(name_tab$x))==0)) {
          unname(as.character(as.matrix(name_tab$x)))
        }else{
          paste0(comp_name,"_effect",if(input$input_variable_no>1){1:nrow(beta_tab$x)})
        }

      # print(reactiveValuesToList(param_list))
      
      ## update equation
      output_list$x <- make_equation(reactiveValuesToList(param_list), print_colours=TRUE)
      
      var_list$x <- simulated_variance(reactiveValuesToList(param_list),data.struc)
      
      ## restore everything

      manyToggle(hide=c("component_type", "input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel"))

       shinyWidgets::updatePickerInput(
        session = session,
        inputId = "component_type",
        selected = ""
      )
      shiny::updateNumericInput(
        session = session,
        inputId = "input_component_name",
        value = ""
      )
      shiny::updateNumericInput(
        session = session,
        inputId = "input_variable_no",
        value = 1
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "input_group",
        selected = ""
      )
    }
  })













#####
# ---  UPDATING COMPONENTS
####

  ### 
  ###

  shiny::observeEvent(input$choose_component, {


    if(input$choose_component %in% c("","intercept","observation","interactions","residual")){
      shinyjs::hide("component_type_edit")
    }else{
      shinyjs::show("component_type_edit")
      shinyjs::hide("input_variable_no_edit")
      shinyjs::hide("beta_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("vcov_panel_edit")
      shinyjs::hide("name_panel_edit")
    }

    if(input$choose_component %in% c("")){
      shinyjs::hide("input_variable_no_edit")
      shinyjs::hide("beta_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("vcov_panel_edit")
      shinyjs::hide("name_panel_edit")
      shinyjs::hide("intercept_panel")
    }

    if(input$choose_component %in% c("intercept")){
      shinyjs::hide("input_variable_no_edit")
      shinyjs::hide("beta_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("vcov_panel_edit")
      shinyjs::hide("name_panel_edit")
      shinyjs::show("intercept_panel")
    }

    if(input$choose_component == c("observation")){
      shinyjs::show("input_variable_no_edit")
      shinyjs::show("name_panel_edit")
      shinyjs::show("mean_panel_edit")
      shinyjs::show("vcov_panel_edit")
      shinyjs::show("beta_panel_edit")
      shinyjs::hide("intercept_panel")
    }
    
    if(input$choose_component == c("interactions")){
      shinyjs::show("input_variable_no_edit")
      shinyjs::show("name_panel_edit")
      shinyjs::show("beta_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("vcov_panel_edit")
      shinyjs::hide("intercept_panel")
    }
    
    if(input$choose_component == c("residual")){
      shinyjs::show("vcov_panel_edit")
      shinyjs::show("beta_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("input_variable_no_edit")
      shinyjs::hide("name_panel_edit")
      shinyjs::hide("intercept_panel")
    }
  
  })
  
  shiny::observeEvent(input$component_type_edit, {
  
   shiny::updateNumericInput(session = session, 
                                  inputId = "input_variable_no_edit", 
                                  value = 1)

    if(input$component_type_edit==c("predictor")){
      shinyjs::show("input_variable_no")
      shinyjs::show("name_panel")
      shinyjs::show("mean_panel")
      shinyjs::show("vcov_panel")
      shinyjs::show("beta_panel")
      shinyjs::hide("intercept_panel")
    }
  
    if(input$component_type_edit==c("random")){
      shinyjs::show("input_variable_no")
      shinyjs::show("vcov_panel")
      shinyjs::show("beta_panel")
      shinyjs::show("name_panel")
      shinyjs::hide("mean_panel")
      shinyjs::hide("intercept_panel")
    }

    if(input$component_type_edit==c("fixed categorical")){
      # set number of 
      # input$input_variable_no <-
      shinyjs::hide("input_variable_no")
      # fill in names from data.str
      # shinyjs::show("name_panel")
      shinyjs::show("name_panel")
      shinyjs::show("beta_panel")
      shinyjs::hide("vcov_panel")     
      shinyjs::hide("mean_panel")
      shinyjs::hide("intercept_panel")
      
      num_level<-length(unique(name_list$x[[component_list$x$group[component_list$x$component==input$choose_component]]]))
      if(num_level>1){
      # if(length(table(name_list$x))>1){
        shiny::updateNumericInput(session = session, 
                                  inputId = "input_variable_no", 
                                  value = num_level)
      }
    }
    
    if(input$component_type_edit==c("covariate")){
      shinyjs::show("name_panel")
      shinyjs::show("beta_panel")
      shinyjs::hide("vcov_panel")     
      shinyjs::hide("mean_panel")
      shinyjs::hide("input_variable_no")
      shinyjs::hide("intercept_panel")
    }
    
    
  })



####
## Pressing update button
####


  #update button updates components
  shiny::observeEvent(input$update_parameters, {
    
    update_comp <- input$choose_component
     
      # work out group name
    update_group <- component_list$x$group[component_list$x$component==update_comp]

    if(input$choose_component == ""){
      shinyalert::shinyalert(title = "Please select a component", type = "error")
    } else if(input$choose_component == "intercept"){
      param_list[["intercept"]] <- input$intercept_panel
    }else{
      
      #make squidSim parameter list 
      param_list[[update_comp]] <- list(
        group = update_group,
        beta = unname(as.matrix(beta_tab$x)),
        mean = as.numeric(as.matrix(mean_tab$x)),
        vcov = unname(as.matrix(vcov_tab$x))
      )
      
      if(update_group != "interactions"){
          param_list[[update_comp]]$fixed <- input$component_type == "fixed categorical"
          param_list[[update_comp]]$covariate <- input$component_type == "covariate"
      }

      ## add in names if they are entered
      param_list[[update_comp]]$names <- 
        if(!all(nchar(as.matrix(name_tab$x))==0)) {
          unname(as.character(as.matrix(name_tab$x)))
        }else{
          paste0(update_comp,"_effect",if(input$input_variable_no>1){1:nrow(beta_tab$x)})
        }
    }

      # print(reactiveValuesToList(param_list))
      
      ## update equation
      output_list$x <- make_equation(reactiveValuesToList(param_list), print_colours=TRUE)
      
      var_list$x <- simulated_variance(reactiveValuesToList(param_list),data.struc)
      
      shinyjs::hide("component_type")
      shinyjs::hide("input_variable_no")
      shinyjs::hide("name_panel")
      shinyjs::hide("mean_panel")
      shinyjs::hide("vcov_panel")
      shinyjs::hide("beta_panel")
      shinyjs::hide("intercept_panel")
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "choose_component",
        selected = ""
      )
 
       shinyWidgets::updatePickerInput(
        session = session,
        inputId = "component_type",
        selected = ""
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "input_variable_no",
        value = 1
      )

  })
  





##############



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
    shiny::withMathJax(paste("$$",output_list$x$equation,"$$"))
  })
  
  output$output_component <- renderText({
    output_list$x$component
  })

  output$output_code <- renderUI({
    HTML(gsub("  ", "&emsp;", gsub(pattern = "\\n", replacement = "<br/>", output_list$x$code)))
  })

  output$output_variance<- renderText({
    var_list$x
  # paste(
  #   "Contribution of the simulated predictors to the mean and variance in the response<br/><br/>",
  #   "Simulated Mean:",var_list$x$total["mean"],"<br/>",
  #   "Simulated Variance:",var_list$x$total["var"],"<br/><br/>",
  #   "Contribution of different hierarchical levels to grand mean and variance:<br/>",
  #   paste(rownames(var_list$x$groups[-1,]),var_list$x$groups[-1,"var"],sep=": ", collapse="<br/>"),
  #   "<br/><br/>Contribution of different predictors to grand mean and variance:<br/>",
  #   paste(rownames(var_list$x$variables[-1,]),var_list$x$variables[-1,"var"],sep=": ", collapse="<br/>")

  # )
  
  })
}