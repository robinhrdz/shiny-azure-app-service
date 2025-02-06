FROM rocker/r-ver:4.2.0

# Install system dependencies for odbc
RUN apt-get update && apt-get install -y \
    unixodbc \
    unixodbc-dev \
    libodbc1 \
    && rm -rf /var/lib/apt/lists/*

# Set working directory and copy files
WORKDIR /app
COPY . /app

# Install renv before restoring packages
RUN R -e "install.packages('renv')"

# Restore packages from renv.lock (if it exists)
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest'); renv::restore()"

# Ensure required packages are installed
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'odbc', 'DBI', 'dplyr', 'ggplot2', 'plotly', 'tidyr', 'lubridate', 'DT'))"

# Expose port 3838 for Shiny apps
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838)"]
