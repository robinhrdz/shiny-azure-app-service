FROM rocker/r-ver:4.2.0


# Install system dependencies for odbc
RUN apt-get update && apt-get install -y \
    unixodbc \
    unixodbc-dev \
    libodbc1 \
    && rm -rf /var/lib/apt/lists/*


COPY . /app
WORKDIR /app

RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest');
          renv::restore()"
          
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'odbc', 'DBI', 'dplyr', 'ggplot2', 'plotly', 'tidyr', 'lubridate', 'DT'))"
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838)"]