# Indicator Visualizations
  <img align="right" src="https://user-images.githubusercontent.com/62613926/193351505-0bfa74f0-60ca-47a3-8895-dec2e9dfbd15.png" width="250" alt="My Image">

## An RShiny user interface for visualizing an integrated ecosystem assessment focused on characterizing the status and trends of the fishery ecosystem in Maine’s coastal waters

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
<br>

### **Included in this Repository is the following:**

###   **1. "Archived" folder containing R code and data used to build preliminary Shiny app for testing**

###   **1. "Code" folder containing R code used to build current Shiny app:**

| File | Description |
| ----------- | ----------- |
|**sea_grant_app.R**| .R that houses code to build and run Shiny app|
|**automatic_ecodata.R**| .R file used to pull relevant data from [ecodata package](https://github.com/NOAA-EDAB/ecodata) and other sources in R, and wrangle data into consistent format to be read in Shiny app <ul><li>this file will be updated such that data pull processes from ecodata can be streamlined & updated on a regular basis</li>|

### **2. "Data" folder containing environmental data and fisheries stock data used in the shiny app:**

| File | Description |
| ----------- | ----------- |
|**stock_assess_data2.csv**| Other stock data used in this app <ul><li>these data are subject to change overtime as app is still in progress</li>|
|**GLORYs_salinity**| Folder containing original .nc slinity data, post-processing .csv salinity data, and metadata .txt file|
|**DisMAP**| Folder containing mean latitude and depth data that were sourced from [DisMAP](https://apps-st.fisheries.noaa.gov/dismap/). Individual files are not discriptivley named as the app is intended to use them as they come named from the original downloads from the site.|

### **3. "Table" folder containing code and information regarding table building process for the RStudio 2022 table competition.**

- This table is included in this repository as it uses publicly available ecosystem indicator data, as used in the app.
  -   The information in this folder is not necessary to run the app, as it is an adjacent project.


#### Any further questions can be directed to Jamie Behan: jbehan@gmri.org
