FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN repository
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))"

# Install all required packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'httr', 'jsonlite', \
                             'dplyr', 'stringr', 'lubridate', 'ggplot2', \
                             'plotly', 'DT', 'shinyjs', 'AzureCosmosR'), dependencies=TRUE)"

# Create app directory and copy app
RUN mkdir /app
COPY app.R /app/
COPY www /app/www/

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838)"]
