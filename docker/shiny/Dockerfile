FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinycssloaders', 'shinyjs', 'flexdashboard', 'rpostgis', 'RPostgres', 'DBI', 'DT', 'crosstalk', 'leaflet', 'RColorBrewer', 'viridis', 'readxl', 'dplyr', 'reactable', 'sparkline', 'ggplot2', 'plotly', 'aqp', 'reactablefmtr'), dependencies=TRUE)"


EXPOSE 3838

CMD ["shiny-server"]
