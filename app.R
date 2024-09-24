# app.R

library(shiny)
library(tidymodels)
library(DT)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Juror Ranking App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("modelFile", "Choose RDS File",
                accept = c(".rds")),
      uiOutput("dataTableUI"),  # Dynamic UI for the table input
      actionButton("rankButton", "Rank Jurors")  # Button to rank jurors
    ),
    
    mainPanel(
      verbatimTextOutput("modelInfo"),
      DTOutput("dataTable")  # Output the editable table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to load the RDS file
  loaded_model <- reactive({
    req(input$modelFile)  # Ensure that a file is selected
    file <- input$modelFile$datapath
    readRDS(file)$juror_model  # Read the model from the RDS file
  })
  
  # Reactive expression to extract predictor names from the tidymodel
  predictor_names <- reactive({
    req(loaded_model())  # Wait for the model to load
    model <- loaded_model()
    
    # Check if model is a workflow and extract the recipe
    if ("workflow" %in% class(model)) {
      model <- extract_recipe(model)
    }
    
    # Extract predictor names
    predictors <- model %>%
      summary() %>%
      filter(role == "predictor") %>% 
      select(variable)
    
    predictors <- predictors$variable
    
    return(predictors)
  })
  
  # Show model information in the UI
  output$modelInfo <- renderPrint({
    req(loaded_model())  # Ensure the model is loaded
    summary(loaded_model())
  })
  
  # Render UI for the editable data table only after predictor names are available
  output$dataTableUI <- renderUI({
    req(predictor_names())  # Ensure predictors are available
    
    # Create a default empty table for data input
    default_data <- data.frame(Name = character(12),
                               matrix(NA, ncol = length(predictor_names()), nrow = 12),
                               stringsAsFactors = FALSE)
    colnames(default_data) <- c("Name", predictor_names())
    
    # Render the editable data table UI
    DTOutput("dataTable")
  })
  
  # Reactive value to store the table data
  table_data <- reactiveVal(NULL)
  
  # Initialize the table data after predictor names are available
  observeEvent(predictor_names(), {
    default_data <- data.frame(Name = character(12),
                               matrix(NA, ncol = length(predictor_names()), nrow = 12),
                               stringsAsFactors = FALSE)
    colnames(default_data) <- c("Name", predictor_names())
    
    # Set the default table data
    table_data(default_data)
  })
  
  # Render the data table after it has been initialized
  output$dataTable <- renderDT({
    req(table_data())  # Ensure table data is available
    
    # Render the editable table
    datatable(table_data(), editable = TRUE)
  }, server = FALSE)
  
  # Capture table edits and update reactive table data
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    table <- table_data()
    table[info$row, info$col] <- info$value
    table_data(table)
  })
  
  # Reactive expression to predict and rank jurors
  ranked_data <- eventReactive(input$rankButton, {
    req(table_data(), predictor_names())  # Ensure table data and predictors are available
    
    # Get the current table data entered by the user
    juror_data <- table_data()
    
    # Remove rows with missing values in predictor columns
    juror_data_clean <- juror_data[complete.cases(juror_data[, predictor_names()]),]
    
    # Run prediction using the tidymodel
    model <- loaded_model()
    predictions <- predict(model, new_data = juror_data_clean)
    
    # Add predictions to the data
    juror_data_clean$Predicted <- predictions$.pred
    
    # Rank jurors based on predictions (best to worst)
    juror_data_clean$Rank <- rank(-juror_data_clean$Predicted, ties.method = "min")
    
    # Return the updated data with ranks
    juror_data_clean
  })
  
  # Update the table with ranks after the "Rank Jurors" button is clicked
  observeEvent(input$rankButton, {
    req(ranked_data())  # Ensure ranked data is available
    
    # Update table data with ranked data
    table_data(ranked_data())
    
    # Re-render the updated table
    output$dataTable <- renderDT({
      datatable(table_data(), editable = TRUE)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
