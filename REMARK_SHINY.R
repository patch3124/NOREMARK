library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Program NOREMARK - Mark-Resight Population Estimation"),
  tags$style(HTML("
    .well { background-color: #f5f5f5; border: 1px solid #e3e3e3; }
    .results-box { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 10px 0; }
    .value-display { font-size: 1.2em; font-weight: bold; color: #2c3e50; }
    .help-text { color: #7f8c8d; font-size: 0.9em; margin-top: 5px; }
    .data-table { width: 100%; border-collapse: collapse; }
    .data-table th { background-color: #e9ecef; text-align: center; padding: 8px; }
    .data-table td { padding: 5px; border: 1px solid #dee2e6; }
    .data-table input { width: 90%; text-align: center; }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      wellPanel(
        h4("Study Parameters"),
        textInput("study_title", "Study Title:", value = "My Mark-Resight Study"),
        numericInput("alpha_level", "Alpha Level:", value = 0.05, min = 0.01, max = 0.50, step = 0.01),
        numericInput("num_occasions", "Number of Sighting Occasions:", value = 5, min = 1, max = 50, step = 1),
        actionButton("setup_data", "Set Up Data Entry", class = "btn-primary"),
        div(class = "help-text", "Set the number of occasions first, then enter data in the table below.")
      ),
      
      wellPanel(
        h4("Data Entry Table"),
        uiOutput("data_table_ui"),
        actionButton("run_analysis", "Run NOREMARK Analysis", class = "btn-success"),
        div(class = "help-text", "Enter data for all occasions, then click to analyze.")
      )
    ),
    
    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "output.results_available",
        h3("Analysis Results"),
        fluidRow(
          column(4, wellPanel(
            h4("Population Estimate (JHE)"),
            div(class = "value-display", textOutput("population_display"))
          )),
          column(4, wellPanel(
            h4("Confidence Interval"),
            div(class = "value-display", textOutput("ci_display"))
          )),
          column(4, wellPanel(
            h4("Minimum Known Alive"),
            div(class = "value-display", textOutput("min_alive_display"))
          ))
        ),
        
        fluidRow(
          column(6, wellPanel(
            h4("Detailed Results"),
            tableOutput("results_table")
          )),
          column(6, wellPanel(
            h4("Mean Chapman Estimate"),
            div(class = "value-display", textOutput("mean_chapman_display")),
            div(class = "help-text", "Average of occasion-specific Chapman estimates")
          ))
        ),
        
        h4("Occasion-Specific Data"),
        DTOutput("occasion_table"),
        
        fluidRow(
          column(12, plotlyOutput("sighting_plot"))
        )
      ),
      
      conditionalPanel(
        condition = "!output.results_available",
        wellPanel(
          h4("Ready for Analysis"),
          p("Please set up your data entry and run the analysis to see results here."),
          p("This implementation uses the Joint Hypergeometric Maximum Likelihood Estimator (JHE)"),
          p("Reference: White, G.C. 1996. Program NOREMARK Software Reference Manual")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  study_data <- reactiveVal()
  analysis_results <- reactiveVal()
  
  # Check if results are available
  output$results_available <- reactive({
    !is.null(analysis_results())
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Data table UI - much better alignment
  output$data_table_ui <- renderUI({
    req(input$num_occasions)
    n <- as.integer(input$num_occasions)
    
    # Create table header
    table_header <- tags$tr(
      tags$th("Occasion"),
      tags$th("Marked Available"),
      tags$th("Marked Seen"),
      tags$th("Unmarked Seen")
    )
    
    # Create table rows
    table_rows <- lapply(1:n, function(i) {
      tags$tr(
        tags$td(style = "text-align: center; font-weight: bold;", i),
        tags$td(numericInput(paste0("marked_avail_", i), label = NULL, 
                             value = 20, min = 0, width = "100%")),
        tags$td(numericInput(paste0("marked_seen_", i), label = NULL, 
                             value = if(i == 1) 10 else 8, min = 0, width = "100%")),
        tags$td(numericInput(paste0("unmarked_seen_", i), label = NULL, 
                             value = if(i == 1) 30 else 25, min = 0, width = "100%"))
      )
    })
    
    # Create the table
    tags$table(
      class = "data-table",
      table_header,
      table_rows
    )
  })
  
  # Collect and analyze data
  observeEvent(input$run_analysis, {
    req(input$num_occasions)
    
    n <- as.integer(input$num_occasions)
    marked_avail <- numeric(n)
    marked_seen <- numeric(n)
    unmarked_seen <- numeric(n)
    
    # Validate all inputs are provided
    valid_inputs <- TRUE
    for(i in 1:n) {
      if(is.null(input[[paste0("marked_avail_", i)]]) || 
         is.null(input[[paste0("marked_seen_", i)]]) || 
         is.null(input[[paste0("unmarked_seen_", i)]])) {
        valid_inputs <- FALSE
        break
      }
      marked_avail[i] <- input[[paste0("marked_avail_", i)]]
      marked_seen[i] <- input[[paste0("marked_seen_", i)]]
      unmarked_seen[i] <- input[[paste0("unmarked_seen_", i)]]
    }
    
    if(!valid_inputs) {
      showNotification("Please fill in all data fields", type = "error")
      return()
    }
    
    # Store data
    study_data(list(
      title = input$study_title,
      alpha = input$alpha_level,
      marked_available = marked_avail,
      marked_seen = marked_seen,
      unmarked_seen = unmarked_seen
    ))
    
    # Run analysis - FIXED: Use alpha directly, not 1-alpha
    tryCatch({
      results <- NOREMARK(
        marked_available = marked_avail,
        marked_seen = marked_seen,
        unmarked_seen = unmarked_seen,
        confidence = 1 - input$alpha_level  # This is correct: alpha=0.05 -> confidence=0.95
      )
      analysis_results(results)
    }, error = function(e) {
      showNotification(paste("Analysis error:", e$message), type = "error")
    })
  })
  
  # Results displays
  output$population_display <- renderText({
    req(analysis_results())
    format(analysis_results()$population_estimate, big.mark = ",")
  })
  
  output$ci_display <- renderText({
    req(analysis_results())
    res <- analysis_results()
    ci_level <- round(res$confidence_level * 100)
    paste0(ci_level, "% CI: ", round(res$confidence_interval["lower"]), " - ", 
           round(res$confidence_interval["upper"]))
  })
  
  output$min_alive_display <- renderText({
    req(analysis_results())
    analysis_results()$minimum_alive
  })
  
  output$mean_chapman_display <- renderText({
    req(analysis_results())
    res <- analysis_results()
    if(is.na(res$mean_chapman_estimate)) {
      "N/A"
    } else {
      format(round(res$mean_chapman_estimate), big.mark = ",")
    }
  })
  
  # Results table
  output$results_table <- renderTable({
    req(analysis_results())
    res <- analysis_results()
    data.frame(
      Parameter = c("Method", "Confidence Level", "Max Log-Likelihood"),
      Value = c(
        "Joint Hypergeometric MLE",
        paste0(round(res$confidence_level * 100), "%"),
        round(res$max_loglikelihood, 2)
      )
    )
  }, bordered = TRUE, align = 'c', width = '100%')
  
  # Occasion data table
  output$occasion_table <- renderDT({
    req(analysis_results())
    res <- analysis_results()$occasion_data
    datatable(
      res,
      options = list(
        pageLength = 10,
        dom = 'tip',
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>% formatRound(columns = c('sighting_rate', 'chapman_estimate', 
                                  'chapman_lower', 'chapman_upper'), digits = 1)
  })
  
  # Single plot for sighting data
  output$sighting_plot <- renderPlotly({
    req(analysis_results())
    res <- analysis_results()$occasion_data
    
    p <- plot_ly(res, x = ~occasion) %>%
      add_trace(y = ~marked_seen, type = 'bar', name = 'Marked Seen', 
                marker = list(color = 'steelblue')) %>%
      add_trace(y = ~unmarked_seen, type = 'bar', name = 'Unmarked Seen',
                marker = list(color = 'indianred')) %>%
      layout(
        title = "Sighting Data by Occasion",
        xaxis = list(title = "Occasion", tickmode = 'linear'),
        yaxis = list(title = "Number of Animals"),
        barmode = 'stack',
        showlegend = TRUE
      )
    p
  })
}

shinyApp(ui, server)
