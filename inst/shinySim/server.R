# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024

server <- function(input, output, session){

  ## list of added components as well as interactions
  component_list <- shiny::reactiveValues(
    x = data.frame(component=c("intercept","residual"),group=c("intercept","residual")))

  all_names <- shiny::reactiveValues(x = c("residual"))

  #table data
  name_tab <- shiny::reactiveValues(x = data.frame(Name = NA),
    edit=NULL)
  beta_tab <- shiny::reactiveValues(x = data.frame(Beta = NA),
    edit=NULL)
  mean_tab <- shiny::reactiveValues(x = data.frame(Mean = NA),
    edit=NULL)
  vcov_tab <- shiny::reactiveValues(x = data.frame(Vcov = NA),
    edit=NULL)

  residual_start = list(vcov = matrix(1), beta=matrix(1), mean=0,group="residual",names="residual", fixed=FALSE, covariate=FALSE)

  #parameter list
  param_list <- shiny::reactiveValues(intercept = 0,residual=residual_start)
  # list containing components, equation and  code for output
  output_list <- shiny::reactiveValues(
    x = make_equation(list(intercept = 0,residual = residual_start))
    )
  var_list <- shiny::reactiveValues(
    x = simVar(list(intercept = 0,residual = residual_start),data.struc)
    )

  #update inputgroup with column headers(wrap in observe event after)
  shiny::observe({

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "input_group",
        choices = c(colnames(data.struc), "observation", "interactions")
      )

  })




#####
# ---  ADDING IN COMPONENT
####


shiny::observeEvent(input$input_group, {

   shiny::updateNumericInput(
    session = session,
    inputId = "input_variable_no",
    value = 1)
    #show or hide group name box if interaction/observation is not picked.
      # print(input$input_group)

    if(input$input_group %in% c("","observation","interactions")){
      manyToggle(hide=c("component_type","input_component_name", "interaction_panel"))
    }else{
      manyToggle(
        show=c("component_type","input_component_name"),
        hide=c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel", "interaction_panel")
      )
    }

    if(input$input_group %in% c("")){
      manyToggle(
        hide=c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel", "interaction_panel")
      )
    }

    if(input$input_group == c("observation")){
      manyToggle(
        show=c("input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel"),
        hide= "interaction_panel"
      )
    }

    if(input$input_group == c("interactions")){
      manyToggle(
        show=c( "interaction_panel", "beta_panel"),
        hide=c("input_variable_no", "mean_panel", "name_panel", "vcov_panel")
      )
    }


  })

  shiny::observeEvent(input$component_type, {

    shiny::updateNumericInput(
      session = session,
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

      num_level<-length(unique(data.struc[[input$input_group]]))
      if(num_level>1){
        shiny::updateNumericInput(
          session = session,
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
        as.character(unique(data.struc[[input$input_group]]))
      }else{
        rep("", num_rows)
      })
      beta_tab$x <- data.frame(Beta = rep(1, num_rows))
      mean_tab$x <- data.frame(Mean = rep(0, num_rows))
      vcov_update <- data.frame(diag(num_rows))
      colnames(vcov_update) <- 1:num_rows
      vcov_tab$x <- vcov_update

    js <- "table.on('click', 'td', function() {
    $(this).dblclick();
  });"


      output$name_table <- DT::renderDT(
        DT::datatable(
          name_tab$x,
          selection = 'none',
          rownames = FALSE,
          colnames = "Name",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          options = list(
            scrollX = TRUE,
            autoWidth = FALSE,
            lengthChange = TRUE,
            dom = "t",
            ordering = FALSE,
            pageLength = 100,
            rowCallback = DT::JS(
              'function(row, data) {',
              '$(row).attr("height", "50px");',
              '$("td", row).css("height", "24px");',
              '$("td", row).on("input", function() {',
              '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();', 
              '  $(this).css("height", emptyCellHeight + "px");', 
              '});',
              '}'
            )
          )
        ) |> DT::formatStyle(1,`text-align` = 'left') |>
          DT::formatStyle(names(name_tab$x), lineHeight = '30px')
      )

# print_table(mean_tab$edit)
# print_table(beta_tab$edit)
# print_table(vcov_tab$edit)

      output$mean_table <- DT::renderDT(
        DT::datatable(
          mean_tab$x,
          rownames = FALSE,
          colnames = "Mean",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          selection = 'none',
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        ) |> DT::formatStyle(1,`text-align` = 'left')
      )


      output$beta_table <- DT::renderDT(
        DT::datatable(
          beta_tab$x,
          editable = list(target = "cell"),
          selection = 'none',
          rownames = FALSE,
          colnames = "Beta",
          callback = DT::JS(js),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100,
                         rowCallback = DT::JS(
                           'function(row, data) {',
                           '$(row).attr("height", "50px");',
                           '$("td", row).css("height", "24px");',
                           '$("td", row).on("input", function() {',
                           '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();', 
                           '  $(this).css("height", emptyCellHeight + "px");', 
                           '});',
                           '}'
                         )
          )
          ) |> DT::formatStyle(1,`text-align` = 'left')
      )

      output$vcov_table <- DT::renderDT(
        DT::datatable(
          vcov_tab$x,
          selection = 'none',
          rownames = FALSE,
          colnames = "VCov",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          options = list(
            scrollX = TRUE,
            autoWidth = FALSE,
            lengthChange = TRUE,
            dom = "t",
            ordering = FALSE,
            pageLength = 100,
            columnDefs = list(list(
              width = paste0(100 / input$input_variable_no, "px"), targets = "_all" 
            )))
        ) |> DT::formatStyle(1:nrow(vcov_tab$x),`text-align` = 'left')
      )

  })

  # table_name,cell_edit_name,reactive_object
  # shiny::observeEvent(input$name_table_cell_edit, {
  #   print(input$name_table_cell_edit)
  #   editTable("name_table",input$name_table_cell_edit,name_tab$x,session = session)
  # })





  ### what happens when add component button is pressed
  shiny::observeEvent(input$add_component, {
    # print(component_list$x)

    comp_name <- if(nchar(input$input_component_name) == 0){
        input$input_group
      }else{
        input$input_component_name
      }
    comp_group <- input$input_group

    int_names <- c(input$int_var1,input$int_var2)
    #all_names$x


    # if(comp_group == "interactions" && ! all(unique(c(strsplit(v_names,":"),recursive=TRUE)) %in% all_names$x) ){
    #   shinyalert::shinyalert(title = "Variables included in interactions are not all specified in the 'names' arguments of other components", type = "error")
    # }else
    if(comp_group == "interactions" && any(int_names=="")) {
      shinyalert::shinyalert(title = "Variables need to be specified ", type = "error")
    }else
    if(input$input_group == ""){
      shinyalert::shinyalert(title = "Please select a group", type = "error")
    }else if(comp_name %in% component_list$x$component){
      shinyalert::shinyalert(title = "Component already added", type = "error")
    }else if(input$component_type == "" & !(input$input_group %in% c("observation", "interactions"))){
      shinyalert::shinyalert(title = "Please select a component type", type = "error")
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

     v_names <- unname(as.character(as.matrix(name_tab$x)))

      ## add in names if they are entered
      param_list[[comp_name]]$names <-
        if(comp_group == "interactions"){
          paste0(input$int_var1,":",input$int_var2)
        }else if(!all(nchar(v_names)==0)) {
          v_names
        }else{
          paste0(comp_name,"_effect",if(input$input_variable_no>1){1:nrow(beta_tab$x)})
        }


      if(comp_group != "interactions"){
        param_list[[comp_name]]$fixed <- input$component_type == "fixed categorical"
        param_list[[comp_name]]$covariate <- input$component_type == "covariate"
        v_names2 <- param_list[[comp_name]]$names
        all_names$x <- c(all_names$x ,v_names2)
        # print(all_names$x)
      }

      # print(reactiveValuesToList(param_list))

      ## update equation
      output_list$x <- make_equation(reactiveValuesToList(param_list), print_colours=TRUE)

      var_list$x <- simVar(reactiveValuesToList(param_list),data.struc)

      ## restore everything

      manyToggle(hide=c("component_type", "input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel"))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "int_var1",
        choices = all_names$x,
        selected = ""
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "int_var2",
        choices = all_names$x,
        selected = ""
      )
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

    update_param <- reactiveValuesToList(param_list)[[input$choose_component]]

   if(input$choose_component %in% c("","intercept","observation","interactions","residual")){
      manyToggle(hide=c("component_type_edit"))
    }else{

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "component_type_edit",
        selected = if(update_param$covariate){"covariate"}else if(update_param$fixed){"fixed categorical"}else{"predictor"}
      )

      if(input$component_type_edit==c("predictor")){
        manyToggle(
          show=c("input_variable_no_edit", "beta_panel_edit", "mean_panel_edit", "vcov_panel_edit", "name_panel_edit","component_type_edit"),
          hide=c("intercept_panel")
        )
      }

      if(input$component_type_edit==c("random")){
        manyToggle(
          show=c("input_variable_no_edit", "beta_panel_edit", "vcov_panel_edit", "name_panel_edit","component_type_edit"),
          hide=c("mean_panel_edit", "intercept_panel")
        )
      }

      if(input$component_type_edit%in%c("fixed categorical","covariate")){
         manyToggle(
          show=c("beta_panel_edit", "name_panel_edit","component_type_edit"),
          hide=c("input_variable_no_edit", "vcov_panel_edit", "mean_panel_edit", "intercept_panel")
        )
      }
    }

    if(input$choose_component == c("")){
      manyToggle(
        hide=c("input_variable_no_edit", "beta_panel_edit", "mean_panel_edit", "vcov_panel_edit", "name_panel_edit","intercept_panel")
      )
    }

    if(input$choose_component == c("observation")){
      manyToggle(
        show=c("input_variable_no_edit", "name_panel_edit", "mean_panel_edit", "vcov_panel_edit", "beta_panel_edit"),
        hide="intercept_panel"
      )
    }

    if(input$choose_component == c("interactions")){
      manyToggle(
        show=c("input_variable_no_edit", "name_panel_edit", "beta_panel_edit"),
        hide=c( "mean_panel_edit", "vcov_panel_edit","intercept_panel")
      )
    }

    if(input$choose_component %in% c("intercept")){
      manyToggle(
        show="intercept_panel",
        hide=c( "input_variable_no_edit", "name_panel_edit", "beta_panel_edit","mean_panel_edit", "vcov_panel_edit")
      )
    }


    if(input$choose_component == c("residual")){
      manyToggle(
        show=c("vcov_panel_edit", "beta_panel_edit"),
        hide=c( "input_variable_no_edit", "name_panel_edit","mean_panel_edit", "intercept_panel")
      )
    }

    if(!input$choose_component %in% c("","intercept")){
# print(reactiveValuesToList(param_list))

      name_tab$edit <- data.frame(Name=update_param$names)
      mean_tab$edit <- data.frame(Mean=update_param$mean)
      beta_tab$edit <- data.frame(Beta=update_param$beta)
      vcov_update <- as.data.frame(update_param$vcov)
      colnames(vcov_update) <- 1:nrow(update_param$vcov)
      vcov_tab$edit <- vcov_update

      shiny::updateNumericInput(
        session = session,
        inputId = "input_variable_no_edit",
        value = nrow(update_param$beta))
    }


    js <- "table.on('click', 'td', function() {
    $(this).dblclick();
  });"

# print_table(beta_tab$edit)
      output$beta_table_edit <- DT::renderDT(
        DT::datatable(
          beta_tab$edit,
          editable = list(target = "cell"),
          selection = 'none',
          rownames = FALSE,
          colnames = "Beta",
          callback = DT::JS(js),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
          ) |> DT::formatStyle(1,`text-align` = 'left')
      )

      output$vcov_table_edit <- DT::renderDT(
        DT::datatable(
          vcov_tab$edit,
          selection = 'none',
          rownames = FALSE,
          colnames = "VCov",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F,pageLength = 100)
        ) |> DT::formatStyle(1:nrow(vcov_tab$edit),`text-align` = 'left')
      )

       output$name_table_edit <- DT::renderDT(
        DT::datatable(
          name_tab$edit,
          selection = 'none',
          rownames = FALSE,
          colnames = "Name",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100,
                         rowCallback = DT::JS(
                           'function(row, data) {',
                           '$("td", row).css("height", "24px");', # Set row height
                           '}'
                         ))
        ) |> DT::formatStyle(1,`text-align` = 'left') |>
          DT::formatStyle(names(name_tab$x), lineHeight = '30px')
      )

      output$mean_table_edit <- DT::renderDT(
        DT::datatable(
          mean_tab$edit,
          rownames = FALSE,
          colnames = "Mean",
          callback = DT::JS(js),
          editable = list(target = "cell"),
          selection = 'none',
          options = list(scrollX = TRUE,autoWidth = FALSE,lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100)
        ) |> DT::formatStyle(1,`text-align` = 'left')
      )
      } )



  # shiny::observeEvent(input$component_type_edit, {

  #   # shiny::updateNumericInput(
  #   #   session = session,
  #   #   inputId = "input_variable_no_edit",
  #   #   value = 1)

  #   if(input$component_type_edit==c("predictor")){
  #     manyToggle(
  #       show=c("component_type_edit","input_variable_no_edit", "beta_panel_edit", "mean_panel_edit", "vcov_panel_edit", "name_panel_edit"),
  #       hide=c("intercept_panel")
  #     )
  #   }

  #   if(input$component_type_edit==c("random")){
  #     manyToggle(
  #       show=c("component_type_edit","input_variable_no_edit", "beta_panel_edit", "vcov_panel_edit", "name_panel_edit"),
  #       hide=c("mean_panel_edit", "intercept_panel")
  #     )
  #   }

  #   if(input$component_type_edit==c("fixed categorical")){
  #     manyToggle(
  #       show=c("component_type_edit","beta_panel_edit","name_panel_edit"),
  #       hide=c("input_variable_no_edit", "vcov_panel_edit", "mean_panel_edit", "intercept_panel")
  #     )

  #     num_level<-length(unique(group_list$x[[component_list$x$group[component_list$x$component==input$choose_component]]]))
  #     if(num_level>1){
  #     # if(length(table(group_list$x))>1){
  #       shiny::updateNumericInput(
  #         session = session,
  #         inputId = "input_variable_no_edit",
  #         value = num_level)
  #     }
  #   }

  #   if(input$component_type_edit==c("covariate")){
  #     manyToggle(
  #       show=c("component_type_edit","beta_panel_edit","name_panel_edit"),
  #       hide=c("input_variable_no_edit", "vcov_panel_edit", "mean_panel_edit", "intercept_panel")
  #     )
  #   }


  # })



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
        beta = unname(as.matrix(beta_tab$edit)),
        mean = as.numeric(as.matrix(mean_tab$edit)),
        vcov = unname(as.matrix(vcov_tab$edit))
      )

      if(update_group != "interactions"){
          param_list[[update_comp]]$fixed <- input$component_type == "fixed categorical"
          param_list[[update_comp]]$covariate <- input$component_type == "covariate"
      }

      ## add in names if they are entered
      param_list[[update_comp]]$names <-
        if(!all(nchar(as.matrix(name_tab$edit))==0)) {
          unname(as.character(as.matrix(name_tab$edit)))
        }else{
          paste0(update_comp,"_effect",if(input$input_variable_no>1){1:nrow(beta_tab$edit)})
        }
    }

      # print(reactiveValuesToList(param_list))

      ## update equation
      output_list$x <- make_equation(reactiveValuesToList(param_list), print_colours=TRUE)

      var_list$x <- simVar(reactiveValuesToList(param_list),data.struc)

      shinyjs::hide("component_type_edit")
      shinyjs::hide("input_variable_no_edit")
      shinyjs::hide("name_panel_edit")
      shinyjs::hide("mean_panel_edit")
      shinyjs::hide("vcov_panel_edit")
      shinyjs::hide("beta_panel_edit")
      shinyjs::hide("intercept_panel")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "choose_component",
        selected = ""
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "component_type_edit",
        selected = ""
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "input_variable_no_edit",
        value = 1
      )

  })





##### Table crap



  #record the data edit
  shiny::observeEvent(input$name_table_cell_edit, {
    proxy_name <- DT::dataTableProxy("name_table")
    info <- input$name_table_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    name_tab$x[i, j] <<- DT::coerceValue(v, name_tab$x[i, j])
    DT::replaceData(DT::dataTableProxy("name_table"), name_tab$x, resetPaging = FALSE)
   # print(name_tab$x)
  })

  #record the data edit
  shiny::observeEvent(input$vcov_table_cell_edit, {
    proxy_vcov <- DT::dataTableProxy("vcov_table")
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
    proxy_beta <- DT::dataTableProxy("beta_table")
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
    proxy_mean <- DT::dataTableProxy("mean_table")
    info <- input$mean_table_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    mean_tab$x[i, j] <<- DT::coerceValue(v, mean_tab$x[i, j])
    DT::replaceData(proxy_mean, mean_tab$x,resetPaging = FALSE)
    str(mean_tab$x)
  })




  #record the data edit
  shiny::observeEvent(input$name_table_edit_cell_edit, {
    proxy_name <- DT::dataTableProxy("name_table_edit")
    info <- input$name_table_edit_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    name_tab$edit[i, j] <<- DT::coerceValue(v, name_tab$edit[i, j])
    DT::replaceData(DT::dataTableProxy("name_table"), name_tab$edit, resetPaging = FALSE)
   # print(name_tab$edit)
  })

  #record the data edit
  shiny::observeEvent(input$vcov_table_edit_cell_edit, {
    proxy_vcov <- DT::dataTableProxy("vcov_table_edit")
    info <- input$vcov_table_edit_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    vcov_tab$edit[i, j] <<- DT::coerceValue(v, vcov_tab$edit[i, j])
    DT::replaceData(proxy_vcov, vcov_tab$edit, resetPaging = FALSE)
    str(vcov_tab$edit)
  })

  #record the data edit
  shiny::observeEvent(input$beta_table_edit_cell_edit, {
    proxy_beta <- DT::dataTableProxy("beta_table_edit")
    info <- input$beta_table_edit_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    beta_tab$edit[i, j] <<- DT::coerceValue(v, beta_tab$edit[i, j])
    DT::replaceData(proxy_beta, beta_tab$edit,resetPaging = FALSE)
    str(beta_tab$edit)
  })

  #record the data edit
  shiny::observeEvent(input$mean_table_edit_cell_edit, {
    proxy_mean <- DT::dataTableProxy("mean_table_edit")
    info <- input$mean_table_edit_cell_edit
    i <- info$row
    j <- info$col+1
    v <- info$value

    mean_tab$edit[i, j] <<- DT::coerceValue(v, mean_tab$edit[i, j])
    DT::replaceData(proxy_mean, mean_tab$edit,resetPaging = FALSE)
    str(mean_tab$edit)
  })



##############

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
  paste(
    "Grand Mean:",var_list$x$total["mean"],"&emsp;",
    "Grand Variance:",var_list$x$total["var"])
  })

  output$output_variance_mid_tab<- shiny::renderTable(var_list$x$groups,rownames = TRUE)

  output$output_variance_mid_plot<- shiny::renderPlot({
    par(mar=c(1,2,1,0.5),bg=NA)
    barplot(matrix(var_list$x$groups$var,dimnames=list(c(rownames(var_list$x$groups)))), beside = FALSE, col=make_colors(rownames(var_list$x$groups)))
  })

  output$output_variance_right_tab<- shiny::renderTable(var_list$x$variables,rownames = TRUE)
  
  output$output_variance_right_plot<- shiny::renderPlot({
    par(mar=c(1,2,1,0.5),bg=NA)
    barplot(matrix(var_list$x$variables$var,dimnames=list(c(rownames(var_list$x$variables)))), beside = FALSE, col=make_colors(rownames(var_list$x$variables)))
  })


  #praising action button + logo leads to citations
  shiny::observeEvent(input$citeme, {
    shinyalert::shinyalert(
      title = "shinySim",
      text = paste(shiny::tags$h5("Made by Ed Ivimey-Cook and Joel Pick")),
      size = "l",
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      animation = TRUE,
      imageUrl =  "squidSim_logo.png",
      imageHeight = "88",
      imageWidth = "80"
    )
  })
}
