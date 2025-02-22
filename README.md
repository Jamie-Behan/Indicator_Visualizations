# Indicator Visualizations
  <img align="right" src="https://user-images.githubusercontent.com/62613926/193351505-0bfa74f0-60ca-47a3-8895-dec2e9dfbd15.png" width="250" alt="My Image">

## An RShiny user interface for visualizing an integrated ecosystem assessment focused on characterizing the status and trends of the fishery ecosystem in Maine’s coastal waters. 
## This RShiny is now live! Explore it [here](https://jamiebehan.shinyapps.io/Monitoring_Ecosystem_Change/)

<img align="left" src="https://github.com/Jamie-Behan/Indicator_Visualizations/blob/main/Visualizing-indicators-gif.gif" width="500" alt="My GIF">

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### **Included in this Repository is the following:**

###   **1. "Archived" folder containing R code and data used to build preliminary Shiny app for testing**

###   **1. "Code" folder containing R code used to build current Shiny app:**

| File | Description |
| ----------- | ----------- |
|**app_metadata.R**| Desriptions of data for the Metadata tab in app|
|**automatic_ecodata.R**| .R file used to pull relevant data from [ecodata package](https://github.com/NOAA-EDAB/ecodata) and other sources in R, and wrangle data into consistent format to be read in Shiny app <ul><li>this file will be updated such that data pull processes from ecodata can be streamlined & updated on a regular basis</li>|
|**Compile_data.R**| .R file that comiles all data sources into one, which is loaded into the sea_grant_app.R|
|**Dismap_data.R**| Wrangling all data sourced from [DisMAP](https://apps-st.fisheries.noaa.gov/dismap/) into usable format for the app. This code should be able to automatically handle updated .csv files if the current files are replaced by new ones from the DisMAP site.|
|**salinity_GLORYs_1993-2020.R**| Wrangling and processing methods for salinity data used in app|
|**sea_grant_app.R**| .R that houses code to build and run Shiny app|
|**supporting_functions_for_app.R**| Funtions called within the app put into the .R file to help clean up sea_grant_app.R script|

### **2. "Data" folder containing environmental data and fisheries stock data used in the shiny app:**

| File | Description |
| ----------- | ----------- |
|**DisMAP**| Folder containing mean latitude and depth data that were sourced from [DisMAP](https://apps-st.fisheries.noaa.gov/dismap/). Individual files are not discriptivley named as the app is intended to use them as they come named from the original downloads from the site.|
|**GLORYs_salinity**| Folder containing original .nc slinity data, post-processing .csv salinity data, and metadata .txt file|
|**stock_assess_data2.csv**| Other stock data used in this app <ul><li>these data are subject to change overtime as app is still in progress</li>|


### **3. "Table" folder containing code and information regarding table building process for the RStudio 2022 table competition.**

- This table is included in this repository as it uses publicly available ecosystem indicator data, as used in the app.
  -   The information in this folder is not necessary to run the app, as it is an adjacent project.


#### Any further questions can be directed to Jamie Behan: jamie.behan@maine.edu
