# Use the rocker/shiny base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libproj-dev \
    gdal-bin \
    libgdal-dev

# Install remotes package to enable installation from GitHub
RUN R -e 'install.packages("remotes")'

# Install additional R packages
RUN R -e 'install.packages(c("dplyr", "ggplot2", "DT","shiny","here", "htmltools","shinydashboardPlus","shinydashboard","plotly","shinyWidgets", "utils","Rcpp","cli","graphics","terra","XML","pacman","stats","readxl"))'

# Install ecodata package from GitHub
RUN R -e 'remotes::install_github("noaa-edab/ecodata", build_vignettes=TRUE)'

# Copy the Shiny app files to the container
COPY app.R /app.R
COPY / Deploy_Shiny

# Copy the CSV files to the container
COPY "mean_salinity_GLORYs.csv" /mean_salinity_GLORYs.csv
COPY "stripedbass_data2.csv" /stripedbass_data2.csv
COPY "Bluefin_stockdata.xlsx" /Bluefin_stockdata.xlsx
COPY "American_plaice.xlsx" /American_plaice.xlsx

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app.R', port = 3838, host = '0.0.0.0')"]



