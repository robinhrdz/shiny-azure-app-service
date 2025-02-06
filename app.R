library(shiny)
library(shinydashboard)
library(odbc)
library(DBI)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(lubridate)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Sacred Marriage Life Program Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Demographics", tabName = "demographics", icon = icon("chart-bar")),
      menuItem("Roles & Access", tabName = "roles", icon = icon("id-badge")),
      menuItem("Registration Trends", tabName = "registrations", icon = icon("calendar")),
      menuItem("Data Tables", tabName = "data_tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(plotlyOutput("users_by_church"), width = 8),
                box(valueBoxOutput("total_users"), valueBoxOutput("active_users"), width = 4)
              )
      ),
      tabItem(tabName = "demographics",
              fluidRow(
                box(plotlyOutput("age_distribution"), width = 6),
                box(plotlyOutput("marital_status_distribution"), width = 6)
              )
      ),
      tabItem(tabName = "roles",
              fluidRow(
                box(plotlyOutput("users_by_role"), width = 12)
              )
      ),
      tabItem(tabName = "registrations",
              fluidRow(
                box(plotlyOutput("daily_registrations"), width = 6),
                box(plotlyOutput("monthly_registration_trend"), width = 6)
              )
      ),
      tabItem(tabName = "data_tables",
              fluidRow(
                box(DT::dataTableOutput("user_details"), width = 12)
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Database connection setup
  con <- dbConnect(odbc(),
                   Driver = "ODBC Driver 18 for SQL Server",
                   Server = "smlp-sqlserver.database.windows.net",
                   Database = "smlp-sqldatabase",
                   UID = "smlp",
                   PWD = "Capstone2",
                   Port = 1433,
                   TrustServerCertificate = "yes")
  
  # Query data
  users <- dbGetQuery(con, "SELECT * FROM dbo.Users")
  churches <- dbGetQuery(con, "SELECT * FROM dbo.Churches")
  roles <- dbGetQuery(con, "SELECT * FROM dbo.Roles")
  
  # Close connection
  dbDisconnect(con)
  
  # Overview - Users by Church
  output$users_by_church <- renderPlotly({
    users %>%
      left_join(churches, by = "church_id") %>%
      count(church_name) %>%
      plot_ly(x = ~church_name, y = ~n, type = "bar",
              marker = list(color = 'rgb(158,202,225)'),
              text = ~n, textposition = 'auto') %>%
      layout(title = "Users per Church",
             xaxis = list(title = "Church"),
             yaxis = list(title = "Number of Users"),
             showlegend = FALSE)
  })
  
  # Overview - Total Users
  output$total_users <- renderValueBox({
    valueBox(nrow(users), icon = "fa-users", color = "primary")
  })
  
  # Overview - Active Users This Month
  output$active_users <- renderValueBox({
    valueBox(
      users %>% 
        filter(month(DateCreated) == month(Sys.Date())) %>% 
        nrow(),
      icon = "fa-user-plus",
      color = "success"
    )
  })
  
  # Demographics - Age Distribution
  output$age_distribution <- renderPlotly({
    users %>%
      mutate(age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")/365.25)) %>%
      plot_ly(x = ~age, type = "histogram", 
              nbinsx = 30,
              marker = list(color = 'rgb(158,202,225)')) %>%
      layout(title = "Age Distribution of Users",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Count"))
  })
  
  # Demographics - Marital Status Distribution
  output$marital_status_distribution <- renderPlotly({
    users %>%
      count(marital_status) %>%
      plot_ly(labels = ~marital_status, 
              values = ~n, 
              type = "pie",
              textinfo = "label+percent") %>%
      layout(title = "Distribution of Marital Status")
  })
  
  # Roles - Users by Role
  output$users_by_role <- renderPlotly({
    users %>%
      left_join(roles, by = "role_id") %>%
      count(role_name) %>%
      plot_ly(x = ~role_name, y = ~n, 
              type = "bar",
              marker = list(color = 'rgb(158,202,225)'),
              text = ~n, textposition = 'auto') %>%
      layout(title = "Users by Role",
             xaxis = list(title = "Role"),
             yaxis = list(title = "Number of Users"),
             showlegend = FALSE)
  })
  
  # Registration Trends - Daily Registrations
  output$daily_registrations <- renderPlotly({
    users %>%
      mutate(date = as.Date(DateCreated)) %>%
      count(date) %>%
      plot_ly(x = ~date, 
              y = ~n, 
              type = "scatter", 
              mode = "lines+markers",
              line = list(color = 'rgb(49,130,189)'),
              marker = list(color = 'rgb(49,130,189)')) %>%
      layout(title = "Daily User Registrations",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Number of Registrations"))
  })
  
  # Registration Trends - Monthly Registration Trend
  output$monthly_registration_trend <- renderPlotly({
    users %>%
      mutate(month = floor_date(DateCreated, "month")) %>%
      count(month) %>%
      plot_ly(x = ~month, 
              y = ~n, 
              type = "bar",
              marker = list(color = 'rgb(158,202,225)'),
              text = ~n, textposition = 'auto') %>%
      layout(title = "Monthly Registration Numbers",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Registrations"),
             showlegend = FALSE)
  })
  
  # Data Tables - User Details
  output$user_details <- DT::renderDataTable({
    users %>%
      left_join(churches, by = "church_id") %>%
      left_join(roles, by = "role_id") %>%
      select(first_name, last_name, email, church_name, role_name, marital_status, date_of_birth, DateCreated) %>%
      arrange(desc(DateCreated)) %>%
      DT::datatable(options = list(pageLength = 10, 
                                   order = list(list(7, 'desc')),
                                   scrollX = TRUE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
