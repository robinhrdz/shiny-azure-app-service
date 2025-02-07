library(shiny)
library(shinydashboard)
library(AzureStor)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Blob Storage Test"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Test Connection", tabName = "test", icon = icon("check"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "test",
              fluidRow(
                box(
                  title = "Connection Status",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("connection_status")
                )
              ),
              fluidRow(
                box(
                  title = "Container Contents",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("container_contents")
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Test connection and get data
  blob_data <- reactive({
    # Create endpoint
    storage_url <- "https://smlpmachinelea8130874026.blob.core.windows.net"
    access_key <- "GOgfse2faDOeieijkH2NfA88uGE2kcyOm5YP+e4J1wF2j2oVRpqNZtNKTxUQPxWm+zB9qoHZVoZi+ASt4+I83A=="  # Replace with your actual access key
    
    tryCatch({
      endpoint <- storage_endpoint(storage_url, key=access_key)
      container <- storage_container(endpoint, "filled-forms")
      blobs <- list_blobs(container)
      return(list(success = TRUE, data = blobs))
    }, error = function(e) {
      return(list(success = FALSE, error = as.character(e)))
    })
  })
  
  # Output connection status
  output$connection_status <- renderPrint({
    result <- blob_data()
    if(result$success) {
      cat("✓ Successfully connected to blob storage!\n")
    } else {
      cat("✗ Connection failed!\n")
      cat("Error: ", result$error)
    }
  })
  
  # Output container contents
  output$container_contents <- renderPrint({
    result <- blob_data()
    if(result$success) {
      cat("Contents of 'filled-forms' container:\n\n")
      print(result$data)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)