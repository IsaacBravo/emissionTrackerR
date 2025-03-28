library(shiny)
library(DT)
library(readr)
library(bslib)

ui <- page_sidebar(
  title = "Local Emissions Tracker Log Viewer",
  sidebar = sidebar(
    helpText("Reads emissions log from your current working directory."),
    verbatimTextOutput("filepath"),
    hr()
  ),
  
  card(
    card_header("Log Data"),
    full_screen = TRUE,
    DTOutput("logTable")
  ),
  
  card(
    card_header("Summary Statistics"),
    layout_columns(
      value_box(
        title = "Total Records",
        value = textOutput("totalRecords"),
        showcase = bsicons::bs_icon("database")
      ),
      value_box(
        title = "Total Emissions",
        value = textOutput("totalEmissions"),
        showcase = bsicons::bs_icon("cloud")
      ),
      value_box(
        title = "Total Energy Consumed (kWh)",
        value = textOutput("totalEnergy"),
        showcase = bsicons::bs_icon("lightning-charge")
      )
    )
  )
)

server <- function(input, output, session) {
  csv_path <- file.path(getwd(), "emissions_log_comparative.csv")
  
  output$filepath <- renderText({
    paste("Reading from:", csv_path)
  })
  
  log_data <- reactive({

    if (file.exists(csv_path)) {
      tryCatch({
        read_csv(csv_path, show_col_types = FALSE)
      }, error = function(e) {
        data.frame(Message = paste("Error reading CSV:", e$message))
      })
    } else {
      data.frame(Message = "CSV file not found. Please ensure 'emissions_log_comparative.csv' exists in your working directory.")
    }
  })
  
  output$logTable <- renderDT({
    data <- log_data()
    datatable(data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 15,
                order = list(0, 'desc'), 
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              filter = 'top'
    )
  })
  
  # Summary statistics
  output$totalRecords <- renderText({
    data <- log_data()
    if (is.data.frame(data) && nrow(data) > 0) {
      nrow(data)
    } else {
      "No data"
    }
  })
  
  output$totalEmissions <- renderText({
    data <- log_data()
    if (is.data.frame(data) && nrow(data) > 0) {
      # Look for emissions column - try common names
      emission_cols <- grep("emissions", colnames(data), ignore.case = TRUE)
      if (length(emission_cols) > 0) {
        # Use the first matching column
        emission_col <- emission_cols[1]
        # Check if the column contains numeric data
        if (is.numeric(data[[emission_col]])) {
          sum_emissions <- sum(data[[emission_col]], na.rm = TRUE)
          # Format with 2 decimal places
          sprintf("%.2f", sum_emissions)
        } else {
          "Non-numeric data"
        }
      } else {
        "No emission data found"
      }
    } else {
      "No data"
    }
  })
  
  output$totalEnergy <- renderText({
    data <- log_data()
    if (is.data.frame(data) && nrow(data) > 0) {
      # Look for energy column - try common names
      energy_cols <- grep("energy|consumption|power|kwh|joule", colnames(data), ignore.case = TRUE)
      if (length(energy_cols) > 0) {
        # Use the first matching column
        energy_col <- energy_cols[1]
        # Check if the column contains numeric data
        if (is.numeric(data[[energy_col]])) {
          sum_energy <- sum(data[[energy_col]], na.rm = TRUE)
          # Format with 2 decimal places
          sprintf("%.2f", sum_energy)
        } else {
          "Non-numeric data"
        }
      } else {
        "No energy data found"
      }
    } else {
      "No data"
    }
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("emissions_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(log_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
