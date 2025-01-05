# UI Component for Single Optimization Module ----
singleOptimizationUI <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Configuration",
      accordion(
        # Function Selection Panel
        accordion_panel(
          "Test Function",
          icon = bsicons::bs_icon("graph-up"),
          selectInput(
            ns("func"),
            "Select Function",
            choices = TEST_FUNCTIONS
          ),
          plotlyOutput(ns("functionPreview"), height = "200px"),
          div(
            class = "mt-3 border rounded p-3",
            textOutput(ns("functionDescription"))
          )
        ),
        
        # Algorithm Settings
        accordion_panel(
          "Algorithm Settings",
          icon = bsicons::bs_icon("sliders"),
          selectInput(
            ns("method"),
            "Optimization Method",
            choices = OPTIMIZATION_METHODS
          ),
          numericInput(
            ns("dims"), 
            "Dimensions", 
            value = DEFAULT_DIMENSIONS, 
            min = 1, 
            max = MAX_DIMENSIONS
          ),
          sliderInput(
            ns("maxiter"),
            "Maximum Iterations",
            min = 100,
            max = 5000,
            value = DEFAULT_MAX_ITERATIONS,
            step = 100
          ),
          numericInput(
            ns("reltol"),
            "Relative Tolerance",
            value = DEFAULT_TOLERANCE,
            min = 1e-16,
            max = 1e-4,
            step = 1e-8
          )
        ),
        
        # Initial Point Settings
        accordion_panel(
          "Initial Point",
          icon = bsicons::bs_icon("cursor-fill"),
          radioButtons(
            ns("init_method"),
            "Selection Method",
            choices = c(
              "Random" = "random",
              "Fixed Distance" = "fixed",
              "Custom" = "custom"
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'fixed'", ns("init_method")),
            numericInput(
              ns("init_distance"),
              "Distance from Optimum",
              value = 2,
              min = 0.1,
              max = 10
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("init_method")),
            textAreaInput(
              ns("custom_init"),
              "Starting Point",
              value = "1,1",
              rows = 2,
              placeholder = "Enter comma-separated values"
            )
          )
        ),
        
        # Method-specific settings
        accordion_panel(
          "Advanced Settings",
          icon = bsicons::bs_icon("gear-fill"),
          checkboxInput(ns("trace"), "Enable Trace", value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'SANN'", ns("method")),
            numericInput(ns("temp"), "Initial Temperature", value = 10),
            numericInput(ns("tmax"), "Temperature Iterations", value = 10)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'CG'", ns("method")),
            selectInput(
              ns("type"), 
              "CG Type",
              choices = c("Fletcher-Reeves", "Polak-Ribiere", "BFGS")
            )
          )
        ),
        accordion_panel(
          "Visualization Settings",
          icon = bsicons::bs_icon("eye"),
          radioButtons(
            ns("viz_type"),
            "Path Visualization",
            choices = c(
              "Contour Plot" = "contour",
              "3D Surface" = "surface"
            ),
            selected = "contour"
          )
        )
      ),
      actionButton(
        ns("optimize"),
        "Run Optimization",
        class = "btn-lg btn-primary w-100 mt-3",
        icon = icon("play")
      )
    ),
    
    # Main Content
    layout_column_wrap(
      width = 1,
      # Results Summary Card
      card(
        card_header(
          class = "bg-primary",
          span(class = "text-white", "Optimization Results")
        ),
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Final Value",
            value = textOutput(ns("finalValue")),
            showcase = bsicons::bs_icon("record-circle"),
            theme = "primary"
          ),
          value_box(
            title = "Distance to Minimum",
            value = textOutput(ns("distanceToMin")),
            showcase = bsicons::bs_icon("arrow-down-circle"),
            theme = "success"
          ),
          value_box(
            title = "Total Iterations",
            value = textOutput(ns("totalIterations")),
            showcase = bsicons::bs_icon("repeat"),
            theme = "info"
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary",
          span(class = "text-white", "Optimization Path")
        ),
        card_body(
          plotlyOutput(ns("optimizationPath"), height = "700px")
        )
      ),
      
      # Visualization Cards
      layout_column_wrap(
        width = 1,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary",
            span(class = "text-white", "Convergence Plot")
          ),
          card_body(
            plotlyOutput(ns("convergencePlot"), height = "500px")
          )
        )
      ),
      
      # Detailed Results Card
      card(
        card_header(
          class = "bg-primary",
          span(class = "text-white", "Detailed Results")
        ),
        card_body(
          div(
            class = "table-responsive",
            tableOutput(ns("results"))
          )
        )
      )
    )
  )
}

# Server Component for Single Optimization Module ----
singleOptimizationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Reactive values ----
    results <- reactiveVal(NULL)
    
    ## Current test function information ----
    current_function <- reactive({
      req(input$func)
      get_function_info(input$func)
    })
    
    ## Reset results when test function changes ----
    observeEvent(input$func, {
      results(NULL)
    })
    
    ## Function preview graph ----
    output$functionPreview <- renderPlotly({
      req(current_function())
      create_landscape_plot(current_function(), "contour", DEFAULT_GRID_SIZE)
    })
    
    ## Function description ----
    output$functionDescription <- renderText({
      req(current_function())
      current_function()$description
    })
    
    ## Initial point validation ----
    observe({
      req(input$init_method == "custom", input$custom_init, input$dims)
      validation <- validate_initial_points(input$custom_init, input$dims)
      if (!validation$valid) {
        showNotification(
          validation$message,
          type = "warning"
        )
      }
    })
    
    ## Method-specific parameter updates ----
    observe({
      req(input$method)
      if (input$method == "SANN") {
        updateNumericInput(session, "maxiter",
                           value = 10000,
                           min = 1000,
                           max = 50000)
      } else {
        updateNumericInput(session, "maxiter",
                           value = DEFAULT_MAX_ITERATIONS,
                           min = 100,
                           max = 5000)
      }
    })
    
    ## Run optimization ----
    observeEvent(input$optimize, {
      withProgress(message = 'Running optimization', value = 0, {
        tryCatch({
          # Get function and initial point
          fn <- current_function()$fn
          x0 <- get_initial_point(
            input$dims,
            input$init_method,
            current_function(),
            input$init_distance,
            input$custom_init
          )
          
          # Set up control parameters
          control_params <- list(
            maxit = input$maxiter,
            reltol = input$reltol,
            trace = input$trace
          )
          
          # Add method-specific parameters
          if (input$method == "SANN") {
            control_params$temp <- input$temp
            control_params$tmax <- input$tmax
          }
          if (input$method == "CG") {
            control_params$type <- input$type
          }
          
          # Create progress updater function
          update_progress <- function(iteration, value) {
            incProgress(
              amount = 1/input$maxiter,
              detail = sprintf("Iteration %d: %.6g", iteration, value)
            )
          }
          
          # Run optimization
          opt_result <- advanced_optim(
            par = x0,
            fn = fn,
            method = input$method,
            runs = 1,
            progress = update_progress,
            control = control_params
          )[[1]]
          
          results(opt_result)
          
        }, error = function(e) {
          showNotification(
            paste("Optimization error:", e$message),
            type = "error"
          )
        })
      })
    })
    
    ## Output plots ----
    
    ### Convergence plot ----
    output$convergencePlot <- renderPlotly({
      req(results())
      create_convergence_plot(results())
    })
    
    
    ### Optimization (2D/3D) Path ----
    output$optimizationPath <- renderPlotly({
      req(results())
      create_optimization_path_plot(
        results(),
        current_function(),
        plot_type = input$viz_type
      )
    })
    
    ## Final Objective Value ----
    output$finalValue <- renderText({
      req(results())
      format_numeric_output(tail(results()$values, 1))
    })
    
    ## Distance to minimum ----
    output$distanceToMin <- renderText({
      req(results(), current_function())
      format_numeric_output(
        abs(tail(results()$values, 1) - current_function()$minimum)
      )
    })
    
    ## Number of Iterations ----
    output$totalIterations <- renderText({
      req(results())
      format_numeric_output(length(results()$values))
    })
    
    ## Detailed results table ----
    output$results <- renderTable({
      req(results())
      data.frame(
        Metric = c(
          "Final Parameters",
          "Initial Value",
          "Final Value",
          "Improvement Factor",
          "Function Evaluations",
          "Convergence Status",
          "Method",
          "Dimensions"
        ),
        Value = c(
          paste(format(results()$par, digits = 6), collapse = ", "),
          format(results()$values[1], digits = 6),
          format(tail(results()$values, 1), digits = 6),
          format(results()$values[1] / tail(results()$values, 1), digits = 6),
          as.character(length(results()$values)),
          ifelse(results()$convergence == 0, "Converged", "Not Converged"),
          input$method,
          as.character(input$dims)
        )
      )
    }, align = 'lr', width = "100%")
  })
}