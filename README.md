# Indicator Visualizations
## An RShiny user interface for visualizing an integrated ecosystem assessment focused on characterizing the status and trends of the fishery ecosystem in Maine’s coastal waters
 
  <img align="right" src="https://user-images.githubusercontent.com/62613926/193351505-0bfa74f0-60ca-47a3-8895-dec2e9dfbd15.png" width="250" alt="My Image">

### **Included in this Repository is the following:**

###   **1. "Code" folder containing R code used to build Shiny app:**
  
- "**Indicator_Visualizations_app.R**" - .R that houses code to build and run Shiny app
        
- "**making_big_ecodata_df.R**" - .R file used to pull relevant data from [ecodata package](https://github.com/NOAA-EDAB/ecodata) in R, and wrangle data into consistent format to be read in Shiny app
    -   *this file will be updated such that data pull processes from ecodata can be streamlined & updated on a regular basis

### **2. "Data" folder containing environmental data and fisheries stock data used in the shiny app:**
   
- "**Plaice_data_dependentvar.xlsx**" - American plaice stock data

- "**big_ecodata.csv**" - Environmental data sourced from the ecodata package in R

- "**stock_assess_data.csv**" - Other stock data used as an example/placeholder in this app
  -   *these data are subject to change overtime as app is still in progress

### **3. "Table" folder containing code and information regarding table building process for the RStudio 2022 table competition.**

- This table is included in this repository as it uses publicacly available ecosystem indicator data, as used in the app.
  -   The information in this folder is not necessary to run the app, as it is an adjacent project.


#### Any further questions can be directed to Jamie Behan: jbehan@gmri.org
