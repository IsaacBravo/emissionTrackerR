
library(shiny)
library(ggplot2)
library(DT)
library(readr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Emission Tracker Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("logfile", "Choose Emissions CSV File", accept = ".csv"),
      dateRangeInput("daterange", "Filter by Date", start = NULL, end = NULL),
      selectInput("project_filter", "Project Name", choices = NULL, multiple = TRUE),
      selectInput("country_filter", "Country", choices = NULL, multiple = TRUE),
      actionButton("reset_filters", "Reset Filters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summaryStats")),
        tabPanel("Plot", plotOutput("emissionsPlot")),
        tabPanel("Table", DTOutput("emissionsTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  log_data <- reactiveVal(NULL)

  observeEvent(input$logfile, {
    df <- read_csv(input$logfile$datapath, show_col_types = FALSE)
    df$timestamp <- as.POSIXct(df$timestamp)
    log_data(df)

    updateSelectInput(session, "project_filter", choices = unique(df$project_name))
    updateSelectInput(session, "country_filter", choices = unique(df$country_name))
    updateDateRangeInput(session, "daterange", start = min(df$timestamp), end = max(df$timestamp))
  })

  observeEvent(input$reset_filters, {
    df <- log_data()
    updateSelectInput(session, "project_filter", selected = character(0))
    updateSelectInput(session, "country_filter", selected = character(0))
    updateDateRangeInput(session, "daterange", start = min(df$timestamp), end = max(df$timestamp))
  })

  filtered_data <- reactive({
    req(log_data())
    df <- log_data()
    if (!is.null(input$daterange[1])) {
      df <- df %>% filter(timestamp >= input$daterange[1], timestamp <= input$daterange[2])
    }
    if (length(input$project_filter) > 0) {
      df <- df %>% filter(project_name %in% input$project_filter)
    }
    if (length(input$country_filter) > 0) {
      df <- df %>% filter(country_name %in% input$country_filter)
    }
    df
  })

  output$summaryStats <- renderPrint({
    summary(select(filtered_data(), emissions, duration, emissions_rate))
  })

  output$emissionsPlot <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = timestamp, y = emissions, color = project_name)) +
      geom_line() +
      geom_point(size = 2) +
      labs(title = "Emissions Over Time", x = "Time", y = "Emissions (kg CO₂eq)") +
      theme_minimal()
  })

  output$emissionsTable <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
