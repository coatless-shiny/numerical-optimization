# UI for the landscape analysis module ----
landscapeAnalysisUI <- function(id) {
  ns <- NS(id)
  
  ## Sidebar layout ----
  layout_sidebar(
    sidebar = sidebar(
      title = "Analysis Settings",
      
      ### Function Selection ----
      selectInput(ns("func"), "Test Function", choices = TEST_FUNCTIONS),
      
      ### Analysis Type ----
      radioButtons(
        ns("analysis_type"),
        "Analysis Type",
        choices = c(
          "Contour Plot" = "contour",
          "3D Surface" = "surface",
          "Gradient Field" = "gradient"
        )
      ),
      
      ### Gradient Field specific settings  ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gradient'", ns("analysis_type")),
        checkboxInput(ns("show_contours"), "Show Contour Lines", value = TRUE)
      ),
      
      ### Resolution Settings  ----
      sliderInput(
        ns("grid_size"),
        "Grid Resolution",
        min = GRID_RESOLUTION_RANGE[1],
        max = GRID_RESOLUTION_RANGE[2],
        value = DEFAULT_GRID_SIZE
      ),
      
      ### Run Button  ----
      actionButton(
        ns("analyze"),
        "Generate Analysis",
        class = "btn-lg btn-primary w-100",
        icon = icon("calculator")
      )
    ),
    
    ## Main content  ----
    layout_column_wrap(
      width = 1,
      ### Plot card ----
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary",
          span(class = "text-white", "Function Landscape")
        ),
        card_body(
          height = "600px",
          conditionalPanel(
            condition = sprintf("!output['%s']", ns("hasAnalysis")),
            div(
              class = "text-center text-muted p-4",
              "Click 'Generate Analysis' to visualize the function landscape"
            )
          ),
          plotlyOutput(ns("landscapePlot"), height = "100%")
        )
      ),
      
      ### Function information section  ----
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary",
          span(class = "text-white", "Function Information")
        ),
        card_body(
          # Formula section
          div(
            class = "mb-4",
            h4("Mathematical Formula", class = "mb-3"),
            div(
              class = "p-3 rounded",
              style = "min-height: 100px; overflow-x: auto;",
              withMathJax(),
              uiOutput(ns("formulaDisplay"))
            )
          ),
          
          #### Function Properties section  ----
          div(
            class = "mb-4",
            h4("Properties", class = "mb-3"),
            div(
              class = "row g-3",
              div(
                class = "col-md-4",
                div(
                  class = "p-3 border rounded h-100",
                  div(class = "d-flex align-items-center mb-2",
                      bsicons::bs_icon("arrows-angle-expand", class = "me-2"),
                      span("Domain", class = "fw-bold")
                  ),
                  textOutput(ns("domainValue"))
                )
              ),
              div(
                class = "col-md-4",
                div(
                  class = "p-3 border rounded h-100",
                  div(class = "d-flex align-items-center mb-2",
                      bsicons::bs_icon("bullseye", class = "me-2"),
                      span("Global Minimum", class = "fw-bold")
                  ),
                  textOutput(ns("minimumValue"))
                )
              ),
              div(
                class = "col-md-4",
                div(
                  class = "p-3 border rounded h-100",
                  div(class = "d-flex align-items-center mb-2",
                      bsicons::bs_icon("geo-alt", class = "me-2"),
                      span("Minimum Location", class = "fw-bold")
                  ),
                  textOutput(ns("minimumLocation"))
                )
              )
            )
          )
          
        )
      )
    )
  )
}

# Server logic for the landscape analysis module ----
landscapeAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Store current function info in a reactive
    current_function <- reactive({ get_function_info(input$func) })
    
    # Reactive value to track if analysis exists
    has_analysis <- reactiveVal(FALSE)
    
    # Expose has_analysis to UI
    output$hasAnalysis <- reactive({
      has_analysis()
    })
    outputOptions(output, "hasAnalysis", suspendWhenHidden = FALSE)
    
    # Render the LaTeX formula
    output$formulaDisplay <- renderUI({
      req(current_function())
      
      withMathJax(
        sprintf(
          "$$%s$$",
          current_function()$latex %||% "\\text{Formula not available}"
        )
      )
    })
    
    # Render domain value
    output$domainValue <- renderText({
      req(current_function())
      func <- current_function()
      sprintf("[%g, %g]", func$domain[1], func$domain[2])
    })
    
    # Render minimum value
    output$minimumValue <- renderText({
      req(current_function())
      sprintf("%g", current_function()$minimum)
    })
    
    # Render minimum location
    output$minimumLocation <- renderText({
      req(current_function())
      func <- current_function()
      if(length(func$minimum_at) <= 6) {
        sprintf("(%s)", paste(sprintf("%g", func$minimum_at), collapse = ", "))
      } else {
        "x* = 0 (all dims)"
      }
    })
    
    # Render function description
    output$functionDescription <- renderText({
      req(current_function())
      current_function()$description
    })
    
    # Clear plot and reset has_analysis when function or analysis type changes
    observeEvent(c(input$func, input$analysis_type), {
      has_analysis(FALSE)
      output$landscapePlot <- renderPlotly({
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          )
      })
    })
    
    # Generate landscape plot only when analyze button is clicked
    observeEvent(input$analyze, {
      req(input$analysis_type, current_function())
      
      withProgress(message = 'Generating analysis', value = 0, {
        output$landscapePlot <- renderPlotly({
          plot <- create_landscape_plot(
            current_function(),
            input$analysis_type,
            input$grid_size,
            input$show_contours
          )
          has_analysis(TRUE)
          plot
        })
      })
    })
  })
}