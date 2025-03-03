library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(httr)
library(jsonlite)

# Custom CSS
customCSS <- HTML("
  .skin-blue .main-header .logo { background-color: #455642 !important; }
  .skin-blue .main-header .navbar { background-color: #455642 !important; }
  .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #3a4837 !important; }
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper { background-color: #2f3b2c !important; }
  .content-wrapper { background-color: #f4f6f4 !important; }
  .box { border-top-color: #455642 !important; }
")

# Function to read and process forms separately
read_all_forms <- function(base_path) {
  form1_data <- list()
  form2_data <- list()
  form3_data <- list()
  
  # List all church directories
  church_dirs <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  
  for (church_dir in church_dirs) {
    church_id <- basename(church_dir)
    user_dirs <- list.dirs(church_dir, full.names = TRUE, recursive = FALSE)
    
    for (user_dir in user_dirs) {
      user_id <- basename(user_dir)
      csv_files <- list.files(user_dir, pattern = "\\.csv$", full.names = TRUE)
      
      for (csv_file in csv_files) {
        tryCatch({
          form_data <- read_csv(csv_file, show_col_types = FALSE)
          form_data$church_id <- church_id
          form_data$user_id <- user_id
          
          # Determine form type and store in appropriate lis
          if ("Most helpful resources" %in% names(form_data)) {
            form1_data[[length(form1_data) + 1]] <- form_data
          } else if ("Spouse is emotionally supportive (1-5)" %in% names(form_data)) {
            form2_data[[length(form2_data) + 1]] <- form_data
          } else if ("Attended marriage retreats" %in% names(form_data)) {
            form3_data[[length(form3_data) + 1]] <- form_data
          }
          
        }, error = function(e) {
          warning(paste("Error reading file:", csv_file))
        })
      }
    }
  }
  
  # Combine each form type separately
  forms <- list(
    form1 = if (length(form1_data) > 0) bind_rows(form1_data) else NULL,
    form2 = if (length(form2_data) > 0) bind_rows(form2_data) else NULL,
    form3 = if (length(form3_data) > 0) bind_rows(form3_data) else NULL
  )
  
  return(forms)
}

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Sacred Marriage Life Program"),
  dashboardSidebar(
    uiOutput("dynamic_menu")
  ),
  dashboardBody(
    tags$head(
      tags$style(customCSS),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL, title = "Current User Information",
          uiOutput("user_display")
        )
      )
    ),
    uiOutput("role_based_content")
  )
)

# Server Definition
server <- function(input, output, session) {
  # Get user data from API
  user_data <- reactiveVal(NULL)
  
  observe({
    response <- tryCatch({
      GET("https://localhost:7271/api/User/get-user", 
          config = list(ssl_verifypeer = FALSE))
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      user_data(fromJSON(rawToChar(response$content)))
    }
  })
  
  # Read form data
  forms_data <- reactive({
    read_all_forms("C:/Users/robin/OneDrive/Desktop/SMLPD/church_form_responses")
  })
  
  # User display
  output$user_display <- renderUI({
    user <- user_data()
    if (is.null(user)) return(NULL)
    
    role_text <- switch(
      as.character(user$role),
      "1" = "Administrator",
      "2" = "Pastor",
      "3" = "User",
      "Unknown Role"
    )
    
    div(
      h3(paste("Welcome", user$name)),
      tags$ul(
        class = "list-group",
        tags$li(class = "list-group-item", tags$strong("Role: "), role_text),
        tags$li(class = "list-group-item", tags$strong("User ID: "), user$userId),
        tags$li(class = "list-group-item", tags$strong("Church ID: "), user$churchId)
      )
    )
  })
  
  # Dynamic menu based on user role
  output$dynamic_menu <- renderMenu({
    user <- user_data()
    if (is.null(user)) return(NULL)
    
    if (user$role == "1") {  # Admin
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("chart-line"))
      )
    } else if (user$role == "2") {  # Pastor
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("chart-line"))
      )
    }
  })
  
  # Role-based content
  output$role_based_content <- renderUI({
    user <- user_data()
    if (is.null(user)) return(NULL)
    
    if (user$role == "1") {  # Admin View
      tabItems(
        # Demographics Tab
        tabItem(tabName = "overview",
                fluidRow(
                  box(title = "Age Distribution", width = 6,
                      plotlyOutput("age_distribution")),
                  box(title = "Income Distribution", width = 6,
                      plotlyOutput("income_distribution"))
                ),
                fluidRow(
                  box(title = "Participants by Church", width = 12,
                      plotlyOutput("church_distribution"))
                ), 
        
        
        # Relationship Health Tab
                fluidRow(
                  box(title = "Overall Satisfaction Metrics", width = 12,
                      plotlyOutput("satisfaction_metrics")),
                  box(title = "Communication and Support", width = 6,
                      plotlyOutput("communication_metrics")),
                  box(title = "Issue Resolution", width = 6,
                      plotlyOutput("issue_resolution"))
                ), 
      
        
        # Church Impact Tab
       
                fluidRow(
                  box(title = "Church Safety Perception", width = 6,
                      plotlyOutput("safety_perception")),
                  box(title = "Marital Care Effectiveness", width = 6,
                      plotlyOutput("care_effectiveness"))
                ),
                fluidRow(
                  box(title = "Church Comparison", width = 12,
                      DTOutput("church_comparison"))
                ),
        
        # Resources Tab
                fluidRow(
                  box(title = "Most Helpful Resources", width = 12,
                      plotlyOutput("helpful_resources")),
                  box(title = "Resource Utilization", width = 6,
                      plotlyOutput("resource_usage")),
                  box(title = "Resource Impact", width = 6,
                      plotlyOutput("resource_impact"))
                )

      )
    )
    } else if (user$role == "2") {  # Pastor View
      tabItems(
        # Overview Tab
        tabItem(tabName = "overview",
                fluidRow(
                  valueBoxOutput("total_members", width = 4),
                  valueBoxOutput("avg_satisfaction", width = 4),
                  valueBoxOutput("needs_attention", width = 4)
                ),
                fluidRow(
                  box(title = "Satisfaction Trends", width = 12,
                      plotlyOutput("church_satisfaction_trend"))
                )
        ),
        
        # Member Support Tab
        tabItem(tabName = "support",
                fluidRow(
                  box(title = "Priority Support Cases", width = 12,
                      DTOutput("priority_cases")),
                  box(title = "Support Needs Distribution", width = 6,
                      plotlyOutput("support_distribution")),
                  box(title = "Issue Categories", width = 6,
                      plotlyOutput("issue_categories"))
                )
        ),
        
        # Pastor Resources Tab
        tabItem(tabName = "pastor_resources",
                fluidRow(
                  box(title = "Resource Effectiveness", width = 12,
                      plotlyOutput("pastor_resource_effectiveness")),
                  box(title = "Recommended Resources", width = 12,
                      DTOutput("recommended_resources"))
                )
        )
      )
    }
  })
  
  # Admin Analytics Outputs
  output$age_distribution <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    plot_ly(data, x = ~Age, type = "histogram",
            marker = list(color = "#455642")) %>%
      layout(title = "Age Distribution",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Count"))
  })
  
  output$income_distribution <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    # Create income distribution plot using Salary column
    plot_ly(data, x = ~Salary, type = "histogram",
            marker = list(color = "#455642"),
            nbinsx = 10) %>%
      layout(title = "Salary Distribution",
             xaxis = list(title = "Monthly Salary"),
             yaxis = list(title = "Count"),
             bargap = 0.1) %>%
      config(displayModeBar = FALSE)
  })
  output$satisfaction_metrics <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    plot_ly(data, x = ~Age, y = ~`General satisfaction (1-5)`,
            type = "scatter", mode = "markers",
            marker = list(color = "#455642", size = 10)) %>%
      layout(title = "Satisfaction by Age",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Satisfaction Level"))
  })
  # Participants by Church Distribution
  output$church_distribution <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    # Count participants per church
    church_counts <- data %>%
      group_by(church_id) %>%
      summarise(count = n()) %>%
      mutate(church_id = paste("Church", church_id))
    
    plot_ly(church_counts, x = ~church_id, y = ~count, type = "bar",
            marker = list(color = "#455642")) %>%
      layout(title = "Participants by Church",
             xaxis = list(title = "Church"),
             yaxis = list(title = "Number of Participants"))
  })
  
  # Communication and Support
  output$communication_metrics <- renderPlotly({
    data <- forms_data()$form2  # Assuming these metrics are in form2
    if (is.null(data)) return(NULL)
    
    plot_ly(data) %>%
      add_trace(y = ~`Communication satisfaction (1-7)`, name = "Communication",
                type = "box", boxpoints = "all", jitter = 0.3,
                marker = list(color = "#455642")) %>%
      add_trace(y = ~`Spouse is emotionally supportive (1-5)`, name = "Support",
                type = "box", boxpoints = "all", jitter = 0.3,
                marker = list(color = "#698964")) %>%
      layout(title = "Communication and Support Metrics",
             yaxis = list(title = "Rating"))
  })
  
  # Issue Resolution
  output$issue_resolution <- renderPlotly({
    data <- forms_data()$form2
    if (is.null(data)) return(NULL)
    
    plot_ly(data, x = ~`Conflict resolution effectiveness (1-7)`, type = "histogram",
            marker = list(color = "#455642")) %>%
      layout(title = "Conflict Resolution Effectiveness",
             xaxis = list(title = "Rating (1-7)"),
             yaxis = list(title = "Count"))
  })
  
  # Church Safety Perception
  output$safety_perception <- renderPlotly({
    data <- forms_data()$form3
    if (is.null(data)) return(NULL)
    
    plot_ly(data, x = ~`Church is a safe place (1-7)`, type = "histogram",
            marker = list(color = "#455642")) %>%
      layout(title = "Church Safety Perception",
             xaxis = list(title = "Rating (1-7)"),
             yaxis = list(title = "Count"))
  })
  
  # Marital Care Effectiveness
  output$care_effectiveness <- renderPlotly({
    data <- forms_data()$form3
    if (is.null(data)) return(NULL)
    
    plot_ly(data, x = ~`Marital care was helpful (1-7)`, type = "histogram",
            marker = list(color = "#455642")) %>%
      layout(title = "Marital Care Effectiveness",
             xaxis = list(title = "Rating (1-7)"),
             yaxis = list(title = "Count"))
  })
  
  # Church Comparison
  output$church_comparison <- renderDT({
    data <- forms_data()$form3
    if (is.null(data)) return(NULL)
    
    comparison_data <- data %>%
      group_by(church_id) %>%
      summarise(
        `Avg Safety Rating` = mean(`Church is a safe place (1-7)`, na.rm = TRUE),
        `Avg Care Effectiveness` = mean(`Marital care was helpful (1-7)`, na.rm = TRUE),
        `Marriage Counseling Available` = sum(`Church provides marriage counseling (Yes/No)` == "Yes") / n() * 100
      ) %>%
      mutate(across(where(is.numeric), round, 2))
    
    datatable(comparison_data,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Most Helpful Resources
  output$helpful_resources <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    resource_counts <- data %>%
      group_by(`Most helpful resources`) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    plot_ly(resource_counts, x = ~`Most helpful resources`, y = ~count, type = "bar",
            marker = list(color = "#455642")) %>%
      layout(title = "Most Helpful Resources",
             xaxis = list(title = "Resource Type"),
             yaxis = list(title = "Number of Participants"))
  })
  
  # Pastor Analytics Outputs
  output$priority_cases <- renderDT({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    user <- user_data()
    church_data <- filter(data, church_id == user$churchId)
    
    priority_data <- church_data %>%
      select(user_id, Age, `General satisfaction (1-5)`, 
             `Happiness in relationship (0-6)`) %>%
      arrange(`General satisfaction (1-5)`)
    
    datatable(priority_data,
              options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  output$church_satisfaction_trend <- renderPlotly({
    data <- forms_data()$form1
    if (is.null(data)) return(NULL)
    
    user <- user_data()
    church_data <- filter(data, church_id == user$churchId)
    
    plot_ly(church_data, x = ~Age, y = ~`General satisfaction (1-5)`,
            type = "scatter", mode = "markers+lines",
            marker = list(color = "#455642")) %>%
      layout(title = "Church Satisfaction Trend",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Satisfaction Level"))
  })
  
  # Add more outputs as needed...
}

# Run the application
shinyApp(ui = ui, server = server)