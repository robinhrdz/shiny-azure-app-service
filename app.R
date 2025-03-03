library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(shinyjs)
library(AzureCosmosR)



# Cosmos DB Connection Details
cosmos_endpoint <- "https://smlp-cosmos.documents.azure.com:443/"
cosmos_key <- "cnIOBAncXO4j4aAxs9hdL8tCsyJHpKQcNuFIBnnrFWQGT7L6H0RthRfwijFEO0GSpl39pLE1lQ2pACDbA90tsw=="
cosmos_database <- "smlp"
cosmos_container_name <- "form-templates"

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Sacred Marriage Life Program", titleWidth = 280),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    ),
    
    # First row with user information - shown to ALL users
    fluidRow(
      column(width = 12,
             box(
               width = NULL, title = "Current User Information", 
               solidHeader = TRUE, status = "primary",
               uiOutput("user_display")
             )
      )
    ),
    
    # Welcome message based on role
    uiOutput("role_welcome_message"),
    
    # Admin-only content - all wrapped in a conditional panel
    uiOutput("admin_dashboard")
  )
)

# Define Server
server <- function(input, output, session) {
  # Cosmos
  template_count <- reactiveVal(0)
  templates_in_use <- reactiveVal(0)
  templates_not_in_use <- reactiveVal(0)
  
  # Function to get count using REST API directly
  getContainerResourceInfo <- function() {
    tryCatch({
      cosmos <- cosmos_endpoint(cosmos_endpoint, cosmos_key)
      db <- get_cosmos_database(cosmos, cosmos_database)
      container <- get_cosmos_container(db, cosmos_container_name)
      
      query_result <- query_documents(container, query = "SELECT * FROM c")
      
      doc_count <- ifelse(is.null(query_result) || nrow(query_result) == 0, 0, nrow(query_result))
      
      in_use_count <- sum(query_result$in_use, na.rm = TRUE)
      not_in_use_count <- doc_count - in_use_count
      
      template_count(doc_count)
      templates_in_use(in_use_count)
      templates_not_in_use(not_in_use_count)
    }, error = function(e) {
      message(paste("Error getting container info:", e$message))
    })
  }
  
  # Initial connection
  observe({
    tryCatch({
      getContainerResourceInfo()
    }, error = function(e) {
      message(paste("Error connecting to Cosmos DB:", e$message))
    })
  })
  
  # Fetch user data
  user_data <- reactiveVal(NULL)
  
  observe({
    response <- tryCatch({
      GET("https://smlp-aabqhwdjcbfee2fx.centralus-01.azurewebsites.net/api/User/get-user", 
          config = list(ssl_verifypeer = FALSE))
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      user_data(fromJSON(rawToChar(response$content)))
    }
  })
  # Church Data
  church_data <- reactiveVal(NULL)
  
 # fetching church data
  observe({
    user <- user_data()
    
    # Only proceed if we have valid user data with a churchId
    if (!is.null(user) && !is.null(user$churchId)) {
      response <- tryCatch({
        GET("https://localhost:7006/api/church", 
            config = list(ssl_verifypeer = FALSE))
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(response) && status_code(response) == 200) {
        all_churches <- fromJSON(rawToChar(response$content))
        
        # Filter to get only the church matching the user's churchId
        user_church <- all_churches[all_churches$id == user$churchId, ]
        
        # Set the reactive value to the filtered church data
        if (nrow(user_church) > 0) {
          church_data(user_church)
        } else {
          # If no matching church was found
          church_data(NULL)
        }
      }
    }
  })
  # Display user info, this is shown to ALL users
  output$user_display <- renderUI({
    user <- user_data()
    
    if (is.null(user)) {
      return(div(class = "alert alert-warning", style = "padding: 10px;",
                 icon("exclamation-triangle"), 
                 "No user logged in"))
    }
    
    role_text <- switch(
      as.character(user$role),
      "1" = "Administrator",
      "2" = "Pastor",
      "3" = "User",
      "Unknown"
    )
    
    # Enhanced user display with more details
    div(
      div(style = "text-align: center;",
          h3(style = "margin: 5px 0; color: #455642;", icon("user-circle"), user$name),
          div(style = "display: inline-block; background-color: #455642; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;", 
              role_text),
          hr(style = "margin: 10px 0;"),
          div(style = "display: flex; justify-content: space-between; margin-top: 10px;",
              div(style = "text-align: center; width: 33%;",
                  p(style = "margin: 0; font-size: 12px; color: #666;", "USER ID"),
                  p(style = "margin: 0; font-weight: bold;", user$userId)
              ),
              div(style = "text-align: center; width: 33%;",
                  p(style = "margin: 0; font-size: 12px; color: #666;", "CHURCH ID"),
                  p(style = "margin: 0; font-weight: bold;", user$churchId)
              ),

          )
      )
    )
  })
  

  # Welcome message based on role
  output$role_welcome_message <- renderUI({
    user <- user_data()
    
    if (is.null(user)) {
      return(NULL)
    }
    
    role_text <- switch(
      as.character(user$role),
      "1" = "Administrator",
      "2" = "Pastor",
      "3" = "User",
      "Unknown"
    )
    
    div(class = "role-welcome",
        p(paste0("Welcome, ", user$name, "! You are logged in as a ", role_text, ".")),

    )
  })
  
  # Dashboards
  output$admin_dashboard <- renderUI({
    user <- user_data()
    
    if (user$role == "1") {
      #admin Dashboard
      return(tagList(
        fluidRow(
          column(width = 4, valueBoxOutput("templates_count_box", width = NULL)),
          column(width = 4, valueBoxOutput("templates_in_use_box", width = NULL)),
          column(width = 4, valueBoxOutput("templates_not_in_use_box", width = NULL))
        ),
        fluidRow(
          column(width = 3, box(
            title = "Total Submissions", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            div(class = "text-center", uiOutput("total_submissions_big"))
          )),
          column(width = 3, box(
            title = "Unique Users", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            div(class = "text-center", uiOutput("unique_users_big"))
          )),
          column(width = 6, box(
            title = "Top Active Users", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            tableOutput("top_users")
          ))
        ),
        fluidRow(
          column(width = 6, div(style = "height: 380px;",
                                box(title = "Submissions per Church", width = NULL, height = "100%",
                                    solidHeader = TRUE, status = "primary",
                                    plotlyOutput("submissions_per_church", height = "300px"))
          )),
          column(width = 6, div(style = "height: 300px;",
                                box(title = "Submissions Over Time", width = NULL, height = "100%",
                                    solidHeader = TRUE, status = "primary",
                                    plotlyOutput("submissions_over_time", height = "300px"))
          ))
        ),
        fluidRow(
          column(width = 6, box(
            title = "Marital Status Distribution", width = NULL, height = "250px",
            solidHeader = TRUE, status = "primary",
            plotlyOutput("marital_status_chart", height = "190px"))
          )
        )
      ))
    } 
    # Pastor Dashboard
    # In the pastor dashboard section, modify the return statement to include church info:
    else if (user$role == "2") {
      return(
        fluidRow(
          column(width = 6, box(
            title = "Church Information", width = NULL,
            solidHeader = TRUE, status = "primary",
            
            fluidRow(
              column(4, strong("Church Name:")),
              column(8, textOutput("church_name"))
            ),
            fluidRow(
              column(4, strong("Address:")),
              column(8, textOutput("church_address"))
            ),
          )),
          column(width = 3, box(
            title = "Total Church Members", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            div(class = "text-center", uiOutput("unique_users_big"))
          )),
          column(width = 3, box(
            title = "Top Active Church Members", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            tableOutput("top_users")
          ))
        )
      )
    }
    # User Dashboard
    else if (user$role == "3") {
      return(
        fluidRow(
          column(width = 6, box(
            title = "Church Information", width = NULL, height = "150px",
            solidHeader = TRUE, status = "primary",
            
            fluidRow(
              column(4, strong("Church Name:")),
              column(8, textOutput("church_name"))
            ),
            fluidRow(
              column(4, strong("Address:")),
              column(8, textOutput("church_address"))
            ),
          ))
        )
      )
    }
    
 else {
      return(NULL)  # If user role is unknown, return nothing
    }
  })
  
  
  
  # Value boxes - Admin only
  output$templates_count_box <- renderValueBox({
    valueBox(
      template_count(),
      "Form Templates",
      icon = icon("file-alt"),
      color = "blue",
      width = NULL
    )
  })
  
  output$templates_in_use_box <- renderValueBox({
    valueBox(
      templates_in_use(),
      "In Use",
      icon = icon("check-circle"),
      color = "green",
      width = NULL
    )
  })
  
  output$templates_not_in_use_box <- renderValueBox({
    valueBox(
      templates_not_in_use(),
      "Not In Use",
      icon = icon("times-circle"),
      color = "red",
      width = NULL
    )
  })
  
  # Fetch form data from blob and parse contents
  fetch_form_data <- reactive({
    tryCatch({
      # First get the blob metadata
      blob_response <- GET("https://smlp-aabqhwdjcbfee2fx.centralus-01.azurewebsites.net/api/ConnectionTest/get-data-from-blob",
                           config = list(ssl_verifypeer = FALSE))
      
      if (status_code(blob_response) != 200) {
        warning(paste("Blob metadata API returned status code:", status_code(blob_response)))
        return(NULL)
      }
      
      blob_data <- fromJSON(content(blob_response, "text"))$data
      if (is.null(blob_data)) {
        warning("Blob API returned empty data")
        return(NULL)
      }
      
      # Extract blob names to then fetch actual form content
      processed_data <- data.frame()
      
      # For each blob, try to fetch the actual form content 
      # This would be replaced with actual API call to get form content
      for (i in 1:nrow(blob_data)) {
        blob_name <- blob_data$blobName[i]
        
        # Extract form details from the blob name
        church_id <- str_extract(blob_name, "church[0-9]+") 
        user_id <- str_extract(blob_name, "user[0-9]+")
        timestamp <- str_extract(blob_name, "\\d{14}") %>% ymd_hms()
        
        form_response <- GET(paste0("https://smlp-aabqhwdjcbfee2fx.centralus-01.azurewebsites.net/api/Form/get-form-data?blobName=", blob_name),
                             config = list(ssl_verifypeer = FALSE))
        
        if (status_code(form_response) == 200) {
          form_data <- fromJSON(content(form_response, "text"))
          
         
          row_data <- data.frame(
            blobName = blob_name,
            church = church_id,
            user = user_id,
            timestamp = timestamp,
            form_id = form_data$form_id,
            marital_status = form_data$`marital status`,
            stringsAsFactors = FALSE
          )
          
          processed_data <- rbind(processed_data, row_data)
        }
      }
      
      
      return(processed_data)
    }, error = function(e) {
      warning(paste("Error in form data processing:", e$message))
      
      
    })
  })
  
  # Fetch blob data for original metrics
  fetch_blob_data <- reactive({
    tryCatch({
      response <- GET("https://smlp-aabqhwdjcbfee2fx.centralus-01.azurewebsites.net/api/ConnectionTest/get-data-from-blob",
                      config = list(ssl_verifypeer = FALSE))
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))$data
        if (is.null(data)) {
          warning("API returned empty data")
          return(NULL)
        }
        return(data)
      } else {
        warning(paste("API returned status code:", status_code(response)))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error in API call:", e$message))
      return(NULL)
    })
  })
  
  # Total Submissions - Big Number Format
  output$total_submissions_big <- renderUI({
    data <- fetch_blob_data()
    if (!is.null(data)) {
      total_count <- nrow(data)
      tagList(
        div(class = "big-metric", total_count),
        div(class = "metric-label", "Total Submissions")
      )
    } else {
      div(class = "alert alert-danger", "Error fetching data")
    }
  })
  
  # Unique Users - Big Number Format
  output$unique_users_big <- renderUI({
    data <- fetch_blob_data()
    if (!is.null(data)) {
      unique_users <- n_distinct(str_extract(data$blobName, "user[0-9]+"))
      tagList(
        div(class = "big-metric", unique_users),
        div(class = "metric-label", "Unique Users")
      )
    } else {
      div(class = "alert alert-danger", "Error fetching data")
    }
  })
  
  # Overview - Submissions per Church - improved visualization with less whitespace
  output$submissions_per_church <- renderPlotly({
    data <- fetch_blob_data()
    if (!is.null(data)) {
      church_counts <- data %>%
        # Extract the church ID properly using regex
        mutate(church = str_extract(blobName, "(?<=filled-forms/)[^/]+")) %>%
        count(church, name = "submissions") %>%
        arrange(desc(submissions))
      
      p <- ggplot(church_counts, aes(x = church, y = submissions, fill = church)) +
        geom_bar(stat = "identity") +
        labs(x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 9)
        ) +
        scale_fill_brewer(palette = "Blues")
      
      ggplotly(p) %>% 
        layout(
          margin = list(l = 40, r = 20, b = 40, t = 10, pad = 0),
          autosize = TRUE
        )
    }
  })
  
  # User Engagement - Top Active Users - more compact table
  output$top_users <- renderTable({
    data <- fetch_blob_data()
    if (!is.null(data)) {
      top_users <- data %>%
        mutate(user = str_extract(blobName, "[0-9]+")) %>%
        count(user, name = "submissions") %>%
        arrange(desc(submissions)) %>%
        head(5)
      return(top_users)
    } else {
      return(data.frame(user = NA, submissions = NA))
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # Submission Trends - Over Time - improved visualization with less whitespace
  output$submissions_over_time <- renderPlotly({
    data <- fetch_blob_data()
    if (!is.null(data)) {
      time_data <- data %>%
        mutate(timestamp = str_extract(blobName, "(\\d{14})") %>% ymd_hms()) %>%
        mutate(date = as.Date(timestamp)) %>%
        count(date, name = "submissions")
      
      p <- ggplot(time_data, aes(x = date, y = submissions)) +
        geom_line(color = "#1f77b4", size = 1) +
        geom_point(color = "#ff7f0e", size = 2) +
        labs(x = NULL, y = NULL) +  # Remove axis labels to save space
        theme_minimal() +
        theme(
          plot.margin = margin(0, 0, 0, 0),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9)
        )
      
      ggplotly(p) %>% 
        layout(
          margin = list(l = 40, r = 20, b = 40, t = 10, pad = 0),
          autosize = TRUE
        )
    }
  })
  
  # Marital Status Distribution
  output$marital_status_chart <- renderPlotly({
    form_data <- fetch_form_data()
    if (!is.null(form_data) && nrow(form_data) > 0) {
      # Calculate marital status distribution
      marital_status_counts <- form_data %>%
        count(`marital status`, name = "count") %>%
        arrange(desc(count))
      
      # Create pie chart
      plot_ly(marital_status_counts, labels = ~marital_status, values = ~count, 
              type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(marital_status, ": ", count, " forms"),
              marker = list(colors = colorRampPalette(c("#455642", "#8DAB7F"))(nrow(marital_status_counts)),
                            line = list(color = '#FFFFFF', width = 1))) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.1),
          margin = list(l = 20, r = 20, b = 10, t = 10, pad = 0),
          autosize = TRUE
        )
    }
  })
  
  # CHURCH DATA

  output$church_name <- renderText({
    church <- church_data()
    if (is.null(church) || nrow(church) == 0) return("Not available")
    return(church$churchName)
  })
  
  output$church_address <- renderText({
    church <- church_data()
    if (is.null(church) || nrow(church) == 0) return("Not available") 
    return(church$churchAddress)
  })
  

}



# Run the application
shinyApp(ui = ui, server = server)