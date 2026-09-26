# shinySim created by Ed Ivimey-Cook and Joel Pick. 26th January 2024 - Updated 3 July 2025 #######

server <- function(input, output, session) {
  ####### Reactive values and dataframes and inital startup ######

  # list of added components as well as interactions
  component_list <- shiny::reactiveValues(x = data.frame(
    component = c("intercept", "residual"),
    group = c("intercept", "residual")
  ))

  all_names <- shiny::reactiveValues(x = c("residual"))

  # bumped whenever the Add form is (re)opened or cleared, so its tables are rebuilt with default
  # values even when the number of variables hasn't changed (issue #24)
  table_reset <- shiny::reactiveVal(0)
  reset_tables <- function() table_reset(table_reset() + 1)

  # a variance-covariance matrix squidSim can simulate from: symmetric, no negative variances,
  # covariances no larger than the variances allow (positive semi-definite)
  valid_vcov <- function(m) {
    m <- suppressWarnings(matrix(as.numeric(as.matrix(m)), nrow(m)))
    !anyNA(m) && isSymmetric(unname(m)) && all(eigen(m, symmetric = TRUE, only.values = TRUE)$values > -1e-8)
  }

  # names squidSim accepts for variables: letters, numbers and '_' only, and not a data-structure column
  reserved_names <- c("intercept", "observation", "residual", "interactions")
  bad_var_names <- function(nm) nm[!grepl("^[A-Za-z0-9_]+$", nm) | nm %in% c(colnames(data.struc), setdiff(reserved_names, "residual"))]
  # component names end up as R code (e.g. `individual = list(...)`), so they must be valid names
  bad_comp_name <- function(nm, group) !grepl("^[A-Za-z][A-Za-z0-9_]*$", nm) || (nm %in% reserved_names && nm != group)
  # squidSim's rule for fixed categorical names: they must be the factor's levels, unless the levels
  # are the numbers 1, 2, ..., k, in which case any names can be given (see squidSim's fill_parameters)
  fixed_names_ok <- function(nm, group) {
    lv <- unique(data.struc[[group]])
    # squidSim's own test for numbered levels (also true for text "1", "2", ...)
    setequal(nm, as.character(lv)) || (length(nm) == length(lv) && all(lv %in% seq_along(lv)))
  }
  # levels of a grouping factor in the order squidSim uses them (for numbered levels, beta i is
  # level i) and the app's variance calculation (table() order)
  sorted_levels <- function(x) {
    lv <- unique(x)
    if (all(lv %in% seq_along(lv))) as.character(sort(as.numeric(as.character(lv)))) else as.character(sort(lv))
  }

  # Safety net: before a change is accepted, write the code the app would print, run it through
  # squidSim and compare squidSim's mean and variance with the app's. Returns NULL if all is well,
  # otherwise the problem (squidSim's own error message where there is one).
  squid_check <- function(p) {
    tryCatch({
      env <- new.env()
      eval(parse(text = make_equation(p, print_colours = FALSE)$code), envir = env)
      sim <- suppressMessages(suppressWarnings(
        if (nrow(data.struc) > 0) {
          squidSim::simulate_population(data_structure = data.struc, parameters = env$parameters)
        } else {
          squidSim::simulate_population(n = 100, parameters = env$parameters)
        }
      ))
      # squidSim's reading of the printed code (sim$param), run through the same variance formula as
      # the app. squidSim::simulated_variance() itself fails when every main effect in an interaction
      # has a single variable (it uses sapply), so it isn't called here.
      squid_param <- sim$param
      for (nm in setdiff(names(squid_param), "intercept")) {
        x <- squid_param[[nm]]
        k <- length(x$names)
        if (is.null(x$vcov)) x$vcov <- diag(k)
        if (is.null(x$mean)) x$mean <- rep(0, k)
        x$beta <- matrix(x$beta, nrow = k)
        if (is.null(x$fixed)) x$fixed <- FALSE
        if (is.null(x$covariate)) x$covariate <- FALSE
        squid_param[[nm]] <- x
      }
      squid_total <- simVar(squid_param, data.struc)$total
      app_total <- simVar(p, data.struc)$total
      if (isTRUE(all.equal(unname(squid_total), unname(app_total), tolerance = 1e-6))) {
        NULL
      } else {
        "the simulation code would give a different mean or variance from the one shown in the app"
      }
    }, error = function(e) conditionMessage(e))
  }
  squid_refuse <- function(problem) {
    shinyalert::shinyalert(
      title = "squidSim can't use this",
      text = paste0("The change wasn't made because ", problem, "."),
      type = "error"
    )
  }

  # the interaction pickers must only offer variables that still exist (issue #20)
  refresh_interaction_choices <- function() {
    for (id in c("int_var1", "int_var2")) {
      shinyWidgets::updatePickerInput(session = session, inputId = id, choices = all_names$x, selected = "")
    }
  }

  # remove interactions that use a variable no longer in the model; returns the ones removed
  drop_orphan_interactions <- function() {
    ints <- param_list$x$interactions
    if (is.null(ints)) return(character(0))
    ok <- vapply(strsplit(ints$names, ":"), function(v) all(v %in% all_names$x), logical(1))
    if (all(ok)) return(character(0))
    removed <- ints$names[!ok]
    if (!any(ok)) {
      param_list$x["interactions"] <- NULL
      component_list$x <- component_list$x[component_list$x$component != "interactions", ]
    } else {
      param_list$x$interactions <- list(
        group = "interactions", names = ints$names[ok], beta = ints$beta[ok, , drop = FALSE],
        mean = rep(0, sum(ok)), vcov = diag(sum(ok))
      )
    }
    removed
  }

  # table data
  name_tab <- shiny::reactiveValues(
    x = data.frame(Name = NA),
    edit = NULL
  )

  beta_tab <- shiny::reactiveValues(
    x = data.frame(Beta = NA),
    edit = NULL
  )

  mean_tab <- shiny::reactiveValues(
    x = data.frame(Mean = NA),
    edit = NULL
  )

  vcov_tab <- shiny::reactiveValues(
    x = data.frame(Vcov = NA),
    edit = NULL
  )

  residual_start <- list(
    vcov = matrix(1),
    beta = matrix(1),
    mean = 0,
    group = "residual",
    names = "residual",
    fixed = FALSE,
    covariate = FALSE
  )

  # parameter list
  param_list <- shiny::reactiveValues(x = list(intercept = 0, residual = residual_start))

  # list containing components, equation and  code for output
  output_list <- shiny::reactiveValues(
    x = make_equation(list(intercept = 0, residual = residual_start))
  )
  var_list <- shiny::reactiveValues(
    x = simVar(list(intercept = 0, residual = residual_start), data.struc)
  )

  # update inputgroup with column headers(wrap in observe event after)
  shiny::observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "input_group",
      choices = c(colnames(data.struc), "observation", "interactions")
    )
  })

  ####### Adding in components #######

  shiny::observeEvent(input$input_group, {
    shiny::updateNumericInput(
      session = session,
      inputId = "input_variable_no",
      value = 1
    )
    # a type picked for another group mustn't carry over (it could leave a fixed categorical
    # name table from one group with the wrong number of rows for another)
    shinyWidgets::updatePickerInput(session = session, inputId = "component_type", selected = "")
    reset_tables()
    # show or hide group name box if interaction/observation is not picked.
    if (input$input_group %in% c("", "observation", "interactions")) {
      manyToggle(hide = c("component_type", "input_component_name", "interaction_panel"))
    } else {
      manyToggle(
        show = c("component_type", "input_component_name"),
        hide = c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel", "interaction_panel")
      )
    }

    if (input$input_group %in% c("")) {
      manyToggle(
        hide = c("input_variable_no", "beta_panel", "mean_panel", "vcov_panel", "name_panel", "interaction_panel")
      )
    }

    if (input$input_group == c("observation")) {
      manyToggle(
        show = c("input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel"),
        hide = "interaction_panel"
      )
    }

    if (input$input_group == c("interactions")) {
      manyToggle(
        show = c("interaction_panel", "beta_panel"),
        hide = c("input_variable_no", "mean_panel", "name_panel", "vcov_panel")
      )
    }
  })

  shiny::observeEvent(input$component_type, {
    shiny::updateNumericInput(
      session = session,
      inputId = "input_variable_no",
      value = 1
    )
    reset_tables()

    if (input$component_type == c("predictor")) {
      manyToggle(
        show = c("input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel")
      )
    }

    if (input$component_type == c("random")) {
      manyToggle(
        show = c("input_variable_no", "name_panel", "vcov_panel", "beta_panel"),
        hide = "mean_panel"
      )
    }

    if (input$component_type == c("fixed categorical")) {
      manyToggle(
        show = c("name_panel", "beta_panel"),
        hide = c("input_variable_no", "mean_panel", "vcov_panel")
      )

      num_level <- length(unique(data.struc[[input$input_group]]))
      if (num_level > 1) {
        shiny::updateNumericInput(
          session = session,
          inputId = "input_variable_no",
          value = num_level
        )
      }
    }

    if (input$component_type == c("covariate")) {
      manyToggle(
        show = c("name_panel", "beta_panel"),
        hide = c("input_variable_no", "mean_panel", "vcov_panel")
      )
    }
  })

  ####### Tables based on input #######

  shiny::observeEvent(list(input$input_variable_no, table_reset()), {
    num_rows <- input$input_variable_no
    shiny::req(is.numeric(num_rows), num_rows >= 1)
    is_fixed <- identical(input$component_type, "fixed categorical") && input$input_group %in% colnames(data.struc)
    if (is_fixed) {
      # one row per level of the grouping factor, named after the levels (squidSim matches them)
      fixed_levels <- sorted_levels(data.struc[[input$input_group]])
      num_rows <- length(fixed_levels)
    }

    name_tab$x <- data.frame(Name = if (is_fixed) fixed_levels else rep("", num_rows))
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
        selection = "none",
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
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          ),
          rowCallback = DT::JS(
            "function(row, data) {",
            '$(row).attr("height", "50px");',
            '$("td", row).css("height", "24px");',
            '$("td", row).on("input", function() {',
            '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();',
            '  $(this).css("height", emptyCellHeight + "px");',
            "});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre") |>
        DT::formatStyle(names(name_tab$x), lineHeight = "30px")
    )

    output$mean_table <- DT::renderDT(
      DT::datatable(
        mean_tab$x,
        rownames = FALSE,
        colnames = "Mean",
        callback = DT::JS(js),
        editable = list(target = "cell"),
        selection = "none",
        options = list(
          scrollX = TRUE,
          autoWidth = FALSE,
          lengthChange = TRUE,
          dom = "t",
          ordering = FALSE,
          pageLength = 100,
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre")
    )

    output$beta_table <- DT::renderDT(
      DT::datatable(
        beta_tab$x,
        editable = list(target = "cell"),
        selection = "none",
        rownames = FALSE,
        colnames = "Beta",
        callback = DT::JS(js),
        options = list(
          scrollX = TRUE, autoWidth = FALSE, lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100,
          rowCallback = DT::JS(
            "function(row, data) {",
            '$(row).attr("height", "50px");',
            '$("td", row).css("height", "24px");',
            '$("td", row).on("input", function() {',
            '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();',
            '  $(this).css("height", emptyCellHeight + "px");',
            "});",
            "}"
          ),
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre")
    )

    output$vcov_table <- DT::renderDT(
      DT::datatable(
        vcov_tab$x,
        selection = "none",
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
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1:nrow(vcov_tab$x), `text-align` = "centre")
    )
  })

  ####### What happens when add component button is pressed ######

  shiny::observeEvent(input$add_component, {
    comp_group <- input$input_group
    is_int <- identical(comp_group, "interactions")
    # all interactions are kept together in squidSim's single 'interactions' component (issue #22)
    comp_name <- if (is_int) {
      "interactions"
    } else if (nchar(input$input_component_name) == 0) {
      input$input_group
    } else {
      input$input_component_name
    }
    # observation-level variables are always simulated predictors; ignore any type left over
    # from a group picked earlier
    comp_type <- if (comp_group == "observation") "predictor" else input$component_type

    int_names <- c(input$int_var1, input$int_var2)
    int_name <- paste(int_names, collapse = ":")
    same_int <- function(a, b) identical(sort(strsplit(a, ":")[[1]]), sort(strsplit(b, ":")[[1]]))
    int_exists <- is_int && any(vapply(param_list$x$interactions$names, same_int, logical(1), b = int_name))

    # names typed in the table; blank ones get squidSim-style default names
    n_var <- nrow(beta_tab$x)
    v_names <- trimws(unname(as.character(as.matrix(name_tab$x))))
    v_names[is.na(v_names)] <- ""
    default_names <- paste0(comp_name, "_effect", if (n_var > 1) seq_len(n_var))
    v_names_full <- ifelse(v_names == "", default_names, v_names)

    if (comp_group == "") {
      shinyalert::shinyalert(
        title = "Please select a group",
        type = "error"
      )
    } else if (is_int && any(int_names == "")) {
      shinyalert::shinyalert(
        title = "Choose both variables for the interaction",
        type = "error"
      )
    } else if (int_exists) {
      shinyalert::shinyalert(
        title = "This interaction has already been added",
        text = "Change its beta in the Update tab instead.",
        type = "error"
      )
    } else if (!is_int && bad_comp_name(comp_name, comp_group)) {
      shinyalert::shinyalert(
        title = "Please choose a different component name",
        text = "Use letters, numbers or '_', starting with a letter, and not 'intercept', 'observation', 'residual' or 'interactions'.",
        type = "error"
      )
    } else if (!is_int && comp_name %in% component_list$x$component) {
      shinyalert::shinyalert(
        title = "Component already added",
        type = "error"
      )
    } else if (!is_int && comp_type == "") {
      shinyalert::shinyalert(
        title = "Please select a component type",
        type = "error"
      )
    } else if (!is_int && length(bad_var_names(v_names_full))) {
      shinyalert::shinyalert(
        title = "These names can't be used",
        text = paste0(paste(bad_var_names(v_names_full), collapse = ", "),
                      ": names can only use letters, numbers and '_', and can't be the name of a column in the data structure."),
        type = "error"
      )
    } else if (!is_int && comp_type == "fixed categorical" && !fixed_names_ok(v_names_full, comp_group)) {
      shinyalert::shinyalert(
        title = "Level names must match the data structure",
        text = paste0("For a fixed categorical effect the names are the levels of '", comp_group, "': ",
                      paste(unique(data.struc[[comp_group]]), collapse = ", "),
                      " (they can only be renamed when the levels are numbered 1, 2, 3, ...)."),
        type = "error"
      )
    } else if (anyNA(suppressWarnings(as.numeric(as.matrix(beta_tab$x)))) ||
               (!is_int && comp_type %in% c("predictor", "random") && anyNA(suppressWarnings(as.numeric(as.matrix(mean_tab$x)))))) {
      shinyalert::shinyalert(
        title = "Every beta and mean needs a number",
        type = "error"
      )
    } else if (!is_int && anyDuplicated(v_names_full)) {
      # issue #25: every variable needs its own name
      shinyalert::shinyalert(
        title = "Each variable needs a different name",
        text = paste("Repeated:", paste(unique(v_names_full[duplicated(v_names_full)]), collapse = ", ")),
        type = "error"
      )
    } else if (!is_int && any(v_names_full %in% all_names$x)) {
      shinyalert::shinyalert(
        title = "This name is already used",
        text = paste("Already used:", paste(v_names_full[v_names_full %in% all_names$x], collapse = ", ")),
        type = "error"
      )
    } else if (!is_int && comp_type %in% c("predictor", "random") && !valid_vcov(vcov_tab$x)) {
      shinyalert::shinyalert(
        title = "The variance-covariance matrix isn't valid",
        text = "Variances can't be negative, and each covariance must imply a correlation between -1 and 1 (|cov| no larger than the square root of the product of the two variances).",
        type = "error"
      )
    } else if (!is_int && comp_type == "covariate" && !is.numeric(data.struc[[comp_group]])) {
      # issue #23: squidSim uses the values of the grouping column itself, so they must be numbers
      shinyalert::shinyalert(
        title = "Covariates need a numeric column",
        text = paste0("'", comp_group, "' in the data structure is not numeric, so it can't be used as a covariate. ",
                      "Choose 'predictor' or 'fixed categorical' instead."),
        type = "error"
      )
    } else {
      # the component as squidSim will see it
      if (is_int) {
        # add this interaction to any already added
        old <- param_list$x$interactions
        n_int <- length(old$names) + 1
        new_comp <- list(
          group = "interactions",
          names = c(old$names, int_name),
          beta = rbind(old$beta, unname(as.matrix(beta_tab$x))[1, , drop = FALSE]),
          mean = rep(0, n_int),
          vcov = diag(n_int)
        )
      } else {
        new_comp <- list(
          group = comp_group,
          beta = matrix(as.numeric(as.matrix(beta_tab$x)), ncol = 1),
          mean = as.numeric(as.matrix(mean_tab$x)),
          vcov = unname(as.matrix(vcov_tab$x)),
          names = v_names_full,
          fixed = comp_type == "fixed categorical",
          covariate = comp_type == "covariate"
        )
      }
      candidate <- param_list$x
      candidate[[comp_name]] <- new_comp
      problem <- squid_check(candidate)
      if (!is.null(problem)) {
        squid_refuse(problem)
        return()
      }

      if (!comp_name %in% component_list$x$component) {
        component_list$x <- data.frame(
          component = c(component_list$x$component, comp_name),
          group = c(component_list$x$group, comp_group)
        )
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "choose_component",
        choices = component_list$x$component
      )

      param_list$x <- candidate
      if (!is_int) all_names$x <- c(all_names$x, v_names_full)

      ## update equation
      output_list$x <- make_equation(param_list$x, print_colours = TRUE)
      var_list$x <- simVar(param_list$x, data.struc)

      ## restore everything
      manyToggle(hide = c("component_type", "input_variable_no", "name_panel", "mean_panel", "vcov_panel", "beta_panel"))

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
      reset_tables()
    }
  })

  ####### Updating components #######
  shiny::observeEvent(input$choose_component, {
    update_param <- param_list$x[[input$choose_component]]

    if (input$choose_component %in% c("", "intercept", "observation", "interactions", "residual")) {
      manyToggle(hide = c("component_type_edit_print"))
    } else {
      component_type_edit <- if (update_param$covariate) {
        "covariate"
      } else if (update_param$fixed) {
        "fixed categorical"
      } else {
        "predictor"
      }

      output$component_type_edit_print <- renderText(
        paste(component_type_edit)
      )

      output$component_type_edit_print <- renderText(
        paste(component_type_edit)
      )

      if (component_type_edit == c("predictor")) {
        manyToggle(
          show = c("beta_panel_edit", "mean_panel_edit", "vcov_panel_edit", "name_panel_edit", "component_type_edit_div_print", "component_type_edit_div", "component_type_edit_print"),
          hide = c("intercept_panel")
        )
      }

      if (component_type_edit %in% c("fixed categorical", "covariate")) {
        manyToggle(
          show = c("beta_panel_edit", "name_panel_edit", "component_type_edit_div_print", "component_type_edit_print"),
          hide = c("vcov_panel_edit", "mean_panel_edit", "intercept_panel")
        )
      }
    }

    if (input$choose_component == c("")) {
      manyToggle(
        hide = c("beta_panel_edit", "mean_panel_edit", "vcov_panel_edit", "name_panel_edit", "intercept_panel")
      )
    }

    if (input$choose_component == c("observation")) {
      manyToggle(
        show = c("name_panel_edit", "mean_panel_edit", "vcov_panel_edit", "beta_panel_edit"),
        hide = "intercept_panel"
      )
    }

    if (input$choose_component == c("interactions")) {
      manyToggle(
        show = c("name_panel_edit", "beta_panel_edit"),
        hide = c("mean_panel_edit", "vcov_panel_edit", "intercept_panel")
      )
    }

    if (input$choose_component %in% c("intercept")) {
      manyToggle(
        show = "intercept_panel",
        hide = c("name_panel_edit", "beta_panel_edit", "mean_panel_edit", "vcov_panel_edit")
      )
      shinyjs::disable("delete_parameters")
    }


    if (input$choose_component == c("residual")) {
      manyToggle(
        show = c("vcov_panel_edit", "beta_panel_edit"),
        hide = c("name_panel_edit", "mean_panel_edit", "intercept_panel")
      )
      shinyjs::disable("delete_parameters")
    }

    if (!input$choose_component %in% c("residual", "intercept")) {
      shinyjs::enable("delete_parameters")
    }

    if (!input$choose_component %in% c("", "intercept")) {
      name_tab$edit <- data.frame(Name = update_param$names)
      mean_tab$edit <- data.frame(Mean = update_param$mean)
      beta_tab$edit <- data.frame(Beta = update_param$beta)
      vcov_update <- as.data.frame(update_param$vcov)
      colnames(vcov_update) <- 1:nrow(update_param$vcov)
      vcov_tab$edit <- vcov_update
    }

    js <- "table.on('click', 'td', function() {
    $(this).dblclick();
  });"

    output$beta_table_edit <- DT::renderDT(
      DT::datatable(
        beta_tab$edit,
        editable = list(target = "cell"),
        selection = "none",
        rownames = FALSE,
        colnames = "Beta",
        callback = DT::JS(js),
        options = list(
          scrollX = TRUE, autoWidth = FALSE, lengthChange = TRUE, dom = "t", ordering = F, pageLength = 100,
          rowCallback = DT::JS(
            "function(row, data) {",
            '$(row).attr("height", "50px");',
            '$("td", row).css("height", "24px");',
            '$("td", row).on("input", function() {',
            '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();',
            '  $(this).css("height", emptyCellHeight + "px");',
            "});",
            "}"
          ),
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre")
    )

    output$vcov_table_edit <- DT::renderDT(
      DT::datatable(
        vcov_tab$edit,
        selection = "none",
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
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1:nrow(vcov_tab$edit), `text-align` = "centre")
    )

    output$name_table_edit <- DT::renderDT(
      DT::datatable(
        name_tab$edit,
        selection = "none",
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
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          ),
          rowCallback = DT::JS(
            "function(row, data) {",
            '$(row).attr("height", "50px");',
            '$("td", row).css("height", "24px");',
            '$("td", row).on("input", function() {',
            '  var emptyCellHeight = $(this).closest("table").find("td:empty").height();',
            '  $(this).css("height", emptyCellHeight + "px");',
            "});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre") |>
        DT::formatStyle(names(name_tab$x), lineHeight = "30px")
    )

    output$mean_table_edit <- DT::renderDT(
      DT::datatable(
        mean_tab$edit,
        rownames = FALSE,
        colnames = "Mean",
        callback = DT::JS(js),
        editable = list(target = "cell"),
        selection = "none",
        options = list(
          scrollX = TRUE,
          autoWidth = FALSE,
          lengthChange = TRUE,
          dom = "t",
          ordering = FALSE,
          pageLength = 100,
          columnDefs = list(
            list(width = "100px", targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'text-align': 'center'});",
            "}"
          )
        )
      ) |> DT::formatStyle(1, `text-align` = "centre")
    )
  })


  ####### Pressing update button #######
  # update button updates components
  shiny::observeEvent(input$update_parameters, {
    update_comp <- input$choose_component
    old <- param_list$x[[update_comp]]

    if (update_comp == "") {
      shinyalert::shinyalert(title = "Please select a component", type = "error")
      return()
    } else if (update_comp == "intercept") {
      if (!is.numeric(input$intercept_panel) || is.na(input$intercept_panel)) {
        shinyalert::shinyalert(title = "The intercept needs a number", type = "error")
        return()
      }
      param_list$x[["intercept"]] <- input$intercept_panel
    } else {
      is_int <- update_comp == "interactions"
      new_names <- trimws(unname(as.character(as.matrix(name_tab$edit))))
      new_names[is.na(new_names) | new_names == ""] <- old$names[is.na(new_names) | new_names == ""]
      other_names <- setdiff(all_names$x, old$names)
      new_beta <- suppressWarnings(as.numeric(as.matrix(beta_tab$edit)))
      new_mean <- suppressWarnings(as.numeric(as.matrix(mean_tab$edit)))
      same_int <- function(a, b) identical(sort(strsplit(a, ":")[[1]]), sort(strsplit(b, ":")[[1]]))
      int_dup <- is_int && any(vapply(seq_along(new_names), function(i)
        any(vapply(new_names[-i], same_int, logical(1), b = new_names[i])), logical(1)))
      refuse <- function(title, text = "") {
        shinyalert::shinyalert(title = title, text = text, type = "error")
      }

      if (anyNA(new_beta) || (!is_int && !isTRUE(old$fixed) && !isTRUE(old$covariate) && anyNA(new_mean))) {
        return(refuse("Every beta and mean needs a number"))
      }
      if (is_int) {
        parts <- strsplit(new_names, ":")
        bad <- new_names[!vapply(parts, function(v) length(v) >= 2 && all(v %in% all_names$x), logical(1))]
        if (length(bad)) {
          return(refuse("Interaction names must join existing variables with ':'", paste("Not valid:", paste(bad, collapse = ", "))))
        }
        if (int_dup) return(refuse("The same interaction appears twice"))
      } else if (isTRUE(old$fixed) && !identical(new_names, old$names) && !fixed_names_ok(new_names, old$group)) {
        # squidSim matches fixed categorical names to the levels in the data structure
        return(refuse("Level names can't be changed",
          "For a fixed categorical effect the names are the levels in the data structure (they can only be renamed when the levels are numbered 1, 2, 3, ...)."))
      } else if (length(bad_var_names(setdiff(new_names, "residual")))) {
        return(refuse("These names can't be used", paste0(paste(bad_var_names(setdiff(new_names, "residual")), collapse = ", "),
          ": names can only use letters, numbers and '_', and can't be the name of a column in the data structure.")))
      } else if (anyDuplicated(new_names)) {
        return(refuse("Each variable needs a different name", paste("Repeated:", paste(unique(new_names[duplicated(new_names)]), collapse = ", "))))
      } else if (!isTRUE(old$fixed) && !isTRUE(old$covariate) && !valid_vcov(vcov_tab$edit)) {
        return(refuse("The variance-covariance matrix isn't valid",
          "Variances can't be negative, and each covariance must imply a correlation between -1 and 1 (|cov| no larger than the square root of the product of the two variances)."))
      } else if (any(new_names %in% other_names)) {
        return(refuse("This name is already used", paste("Already used:", paste(new_names[new_names %in% other_names], collapse = ", "))))
      }

      # start from the component as it was, so its group and type (fixed categorical, covariate,
      # predictor) are kept - they were previously taken from the Add tab's type box
      upd <- old
      upd$beta <- matrix(new_beta, ncol = 1)
      upd$names <- new_names
      if (is_int) {
        upd$mean <- rep(0, length(new_names))
        upd$vcov <- diag(length(new_names))
      } else {
        upd$mean <- new_mean
        upd$vcov <- unname(as.matrix(vcov_tab$edit))
      }
      candidate <- param_list$x
      candidate[[update_comp]] <- upd

      renamed <- !is_int && !identical(new_names, old$names)
      if (renamed && !is.null(candidate$interactions)) {
        # renamed variables: follow the new names into any interactions that use them
        lookup <- stats::setNames(new_names, old$names)
        candidate$interactions$names <- vapply(strsplit(candidate$interactions$names, ":"), function(v) {
          v[v %in% names(lookup)] <- lookup[v[v %in% names(lookup)]]
          paste(v, collapse = ":")
        }, character(1))
      }

      problem <- squid_check(candidate)
      if (!is.null(problem)) {
        squid_refuse(problem)
        return()
      }
      param_list$x <- candidate
      if (renamed) {
        all_names$x <- c(other_names, new_names)
        refresh_interaction_choices()
      }
    }

    ## update equation
    output_list$x <- make_equation(param_list$x, print_colours = TRUE)

    var_list$x <- simVar(param_list$x, data.struc)

    shinyjs::hide("component_type_edit_print")
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
  })

  shiny::observeEvent(input$click_component_type, {
    shinyalert::shinyalert(
      title = "Component Types",
      text = "<div>Predictor = Simulates a predictor variable across the specific levels</div>
<div>Fixed Categorical = Specify specific values for each level</div>
<div>Covariate = Uses the levels as a continuous variable</div>", type = "info",
      html = TRUE
    )
  })

  shiny::observeEvent(input$click_group, {
    shinyalert::shinyalert(
      title = "Select Group",
      text = "<div>Select the variable you want to add</div>", type = "info",
      html = TRUE
    )
  })

  shiny::observeEvent(input$click_component_name, {
    shinyalert::shinyalert(
      title = "Add Component name",
      text = "<div>Optional - add a name for the component</div>", type = "info",
      html = TRUE
    )
  })

  shiny::observeEvent(input$click_variable_no, {
    shinyalert::shinyalert(
      title = "Add levels of variable",
      text = "<div>Adjust the number of levels of a component</div>", type = "info",
      html = TRUE
    )
  })

  ####### Pressing Delete button #######
  shiny::observeEvent(input$delete_parameters, {
    delete_comp <- input$choose_component
    if (delete_comp %in% c("", "intercept", "residual")) return()
    delete_names <- param_list$x[[delete_comp]]$names

    if (delete_comp != "interactions") all_names$x <- all_names$x[!all_names$x %in% delete_names]

    param_list$x[delete_comp] <- NULL

    # interactions that used a deleted variable can't be simulated any more (issue #20)
    removed <- drop_orphan_interactions()
    if (length(removed)) {
      shinyalert::shinyalert(
        title = "Interactions removed",
        text = paste("These used a deleted variable:", paste(removed, collapse = ", ")),
        type = "warning"
      )
    }
    refresh_interaction_choices()

    output_list$x <- make_equation(param_list$x, print_colours = TRUE)

    var_list$x <- simVar(param_list$x, data.struc)

    component_list$x <- component_list$x[!component_list$x$component %in% delete_comp, ]

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "choose_component",
      selected = "",
      choices = component_list$x$component
    )
  })

  ####### Table code #######
  # record the data edit
  shiny::observeEvent(input$name_table_cell_edit, {
    proxy_name <- DT::dataTableProxy("name_table")
    info <- input$name_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    name_tab$x[i, j] <<- DT::coerceValue(v, name_tab$x[i, j])
    DT::replaceData(DT::dataTableProxy("name_table"), name_tab$x, resetPaging = FALSE)
  })

  # record the data edit
  shiny::observeEvent(input$vcov_table_cell_edit, {
    proxy_vcov <- DT::dataTableProxy("vcov_table")
    info <- input$vcov_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    vcov_tab$x[i, j] <<- DT::coerceValue(v, vcov_tab$x[i, j])
    # a covariance applies both ways: keep the matrix symmetric, as squidSim requires
    if (i != j && j <= nrow(vcov_tab$x)) vcov_tab$x[j, i] <<- vcov_tab$x[i, j]
    DT::replaceData(proxy_vcov, vcov_tab$x, resetPaging = FALSE)
    str(vcov_tab$x)
  })

  # record the data edit
  shiny::observeEvent(input$beta_table_cell_edit, {
    proxy_beta <- DT::dataTableProxy("beta_table")
    info <- input$beta_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    beta_tab$x[i, j] <<- DT::coerceValue(v, beta_tab$x[i, j])
    DT::replaceData(proxy_beta, beta_tab$x, resetPaging = FALSE)
    str(beta_tab$x)
  })

  # record the data edit
  shiny::observeEvent(input$mean_table_cell_edit, {
    proxy_mean <- DT::dataTableProxy("mean_table")
    info <- input$mean_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    mean_tab$x[i, j] <<- DT::coerceValue(v, mean_tab$x[i, j])
    DT::replaceData(proxy_mean, mean_tab$x, resetPaging = FALSE)
    str(mean_tab$x)
  })

  # record the data edit
  shiny::observeEvent(input$name_table_edit_cell_edit, {
    proxy_name <- DT::dataTableProxy("name_table_edit")
    info <- input$name_table_edit_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    name_tab$edit[i, j] <<- DT::coerceValue(v, name_tab$edit[i, j])
    DT::replaceData(DT::dataTableProxy("name_table_edit"), name_tab$edit, resetPaging = FALSE)
  })

  # record the data edit
  shiny::observeEvent(input$vcov_table_edit_cell_edit, {
    proxy_vcov <- DT::dataTableProxy("vcov_table_edit")
    info <- input$vcov_table_edit_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    vcov_tab$edit[i, j] <<- DT::coerceValue(v, vcov_tab$edit[i, j])
    if (i != j && j <= nrow(vcov_tab$edit)) vcov_tab$edit[j, i] <<- vcov_tab$edit[i, j]
    DT::replaceData(proxy_vcov, vcov_tab$edit, resetPaging = FALSE)
    str(vcov_tab$edit)
  })

  # record the data edit
  shiny::observeEvent(input$beta_table_edit_cell_edit, {
    proxy_beta <- DT::dataTableProxy("beta_table_edit")
    info <- input$beta_table_edit_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    beta_tab$edit[i, j] <<- DT::coerceValue(v, beta_tab$edit[i, j])
    DT::replaceData(proxy_beta, beta_tab$edit, resetPaging = FALSE)
    str(beta_tab$edit)
  })

  # record the data edit
  shiny::observeEvent(input$mean_table_edit_cell_edit, {
    proxy_mean <- DT::dataTableProxy("mean_table_edit")
    info <- input$mean_table_edit_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value

    mean_tab$edit[i, j] <<- DT::coerceValue(v, mean_tab$edit[i, j])
    DT::replaceData(proxy_mean, mean_tab$edit, resetPaging = FALSE)
    str(mean_tab$edit)
  })

  ####### Outputs to display #######
  
  #changing tabs
  observeEvent(input$add_update_tab, {
    req(input$add_update_tab)
    if(input$add_update_tab == "Add") {
      shinyjs::reset("add_tab_container")
      reset_tables()
    }
    if(input$add_update_tab == "Update") {
      shinyjs::reset("update_tab_container")
    }
  })

  output$output_equation <- renderUI({
    shiny::withMathJax(paste("$$", output_list$x$equation, "$$"))
  })

  output$output_component <- renderText({
    output_list$x$component
  })

  # copy the simulation code as plain text (issue #26)
  shiny::observeEvent(input$copy_code, {
    session$sendCustomMessage("copy_to_clipboard", make_equation(param_list$x, print_colours = FALSE)$code)
  })
  shiny::observeEvent(input$copy_code_done, {
    if (identical(input$copy_code_done, "ok")) {
      shiny::showNotification("Code copied - paste it into R", type = "message", duration = 3)
    } else {
      shiny::showNotification("Couldn't copy automatically - select the code and copy it instead", type = "warning")
    }
  })

  output$output_code <- renderUI({
    HTML(gsub("  ", "&emsp;", gsub(pattern = "\\n", replacement = "<br/>", output_list$x$code)))
  })

  output$output_variance <- renderText({
    paste(
      "Grand Mean:", var_list$x$total["mean"], "&emsp;",
      "Grand Variance:", var_list$x$total["var"]
    )
  })

  output$output_variance_mid_tab <- shiny::renderTable(var_list$x$groups, rownames = TRUE)

  output$output_variance_mid_plot <- shiny::renderPlot({
    par(mar = c(1, 2, 1, 0.5), bg = NA)
    barplot(matrix(var_list$x$groups$var, dimnames = list(c(rownames(var_list$x$groups)))),
            beside = FALSE,
            col = make_colors(rownames(var_list$x$groups)))
  })

  output$output_variance_right_tab <- shiny::renderTable(var_list$x$variables[, 1:2], rownames = TRUE)

  output$output_variance_right_plot <- shiny::renderPlot({
    par(mar = c(1, 2, 1, 0.5), bg = NA)
    barplot(matrix(var_list$x$variables$var, 
                   dimnames = list(c(rownames(var_list$x$variables)))),
            beside = FALSE, 
            col = make_colors(rownames(var_list$x$groups))[var_list$x$variables[, 3]])
  })

  # praising action button + logo
  shiny::observeEvent(input$citeme, {
    shinyalert::shinyalert(
      title = "shinySim",
      text = paste0(
        shiny::tags$h5("A Shiny version of squidSim made by Ed Ivimey-Cook and Joel Pick"),
        "<br><br>",
        shiny::tags$a(
          href = "https://squidgroup.org/squidSim_vignette",
          target = "_blank",
          "See vignette for detailed information"
        )
      ),
      size = "l",
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      animation = TRUE,
      imageUrl = "squidSim_logo.png",
      imageHeight = "88",
      imageWidth = "80"
    )
  })

  # stop app when sesssion ends
  session$onSessionEnded(function() {
    stopApp()
  })
}
