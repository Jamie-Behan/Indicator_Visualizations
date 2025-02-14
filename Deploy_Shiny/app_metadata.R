#### Visualizing Indicators RShiny metadata #####
library(pacman)
pacman::p_load(htmltools,shiny,shinydashboardPlus,shinydashboard,graphics,install=TRUE)

#####Metadata#####
metadata <-tabItem(tabName = "Metadata",
                   h2("Metadata"),
                   
                   
                   box(
                     accordion(
                       id = "accordion1",
                       accordionItem(
                         title = "Annual AMO:",
                         status = "primary",
                         "AMO (Atlantic Multidecadal Oscillation) Index. These data represent annual means of the NOAA Physical Sciences Laboratory's unsmoothed short monthly AMO dataset found at https://psl.noaa.gov/data/timeseries/AMO/. 1948-present. These data represent the weighted average over the N Atlantic from 0 to 70N, and have been detrended."
                       ),
                       accordionItem(
                         title = "Atlantic Herring Seasonal Latitude and Depth (Prey)",
                         status = "primary",
                         "Atlantic Herring Seasonal Latitude (Decimal Degrees) and Depth (m) are sourced from the NOAA Fisheries' Distribution Mapping and Analysis Portal (DisMAP). Data represent the mean center of gravity (geographic center) metrics from the NEFSC bottom trawl survey. Metrics were calculated as biomass-weighted averages of depth and latitude, weighted by the interpolated biomass at each depth or latitude for each year and season (fall, spring) of the bottom trawl survey.  See https://apps-st.fisheries.noaa.gov/dismap/ for more info. "
                       ),
                       accordionItem(
                         title = "In-situ Bottom Temperature Anomaly (NEFSC)",
                         status = "primary",
                         "Annual bottom temperature anomalies for the GOM region. Bottom Temperature data collected from NEFSC survey from 1977-present. 
                                        In ℃.Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Bottom Temperature Anomaly (GLORYS)",
                         status = "primary",
                         "GLORYS12V1 daily bottom temperature product anomalies for the GOM region. Annual means from 1993-2018. 
                                          1994-2010 climatology was used as anomaly period. In ℃.Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Calanus Abundance Anomaly",
                         status = "primary",
                         "Calanus finmarchicus abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info. "
                       ),
                       
                       accordionItem(
                         title = "Forage Fish Index",
                         status = "primary",
                         "Biomass index of 20 small pelagic forage fish speices. The approach used to generate these data include survey-sampled predator stomach 
                         contents as observations to develop a survey index for forage fish, following Ng et al. (2021). The 20 species selected were targeted 
                         Bluefish (Pomatomus saltatrix) prey fish and include: Longfin squids (Doryteuthis formerly Loligo sp.), Anchovy family (Engraulidae), 
                         bay anchovy (Anchoa mitchilli), Atlantic butterfish, (Peprilus triachanthus), Cephalopoda, (Anchoa hepsetus), red eye round herring 
                         (Etrumeus teres), Sandlance (Ammodytes sp.), scup (Stenotomus chrysops), silver hake (Merluccius bilinearis), shortfin squids (Illex sp.), 
                         Atlantic herring (Clupea harengus), Herring family (Clupeidae), Bluefish (Pomatomus saltatrix), silver anchovy (Engraulis eurystole), 
                         longfin inshore squid (Doryteuthis pealeii), Atlantic mackerel (Scomber scombrus), flatfish (Pleuronectiformes), weakfish (Cynoscion regalis), 
                         and Atlantic menhaden (Brevoortia tyrannus). Predators with highest diet similarity to Bluefish from the NEFSC diet database (1973-2020) 
                         include Atlantic cod, Atlantic halibut, buckler dory, cusk, fourspot flounder, goosefish, longfin squid, shortfin squid, pollock, red hake, 
                         sea raven, silver hake, spiny dogfish, spotted hake, striped bass, summer flounder, thorny skate, weakfish, and white hake.
                         These data were sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/forage_index.html?q=forage#forage_index for more info. "
                       ),
                       
                       accordionItem(
                         title = "Median Cholorphyll A",
                         status = "primary",
                         "time series of remotely sensed chlorophyll a (CHL) from the Northeast Continental Shelf region. Data are from multiple sensors (OC-CCI, SeaWiFS, MODIS-Aqua) and sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info. "
                       ),
                       accordionItem(
                         title = "Large Copepod Abundance Anomalies",
                         status = "primary",
                         "Large copepod abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Small Copepod Abundance Anomalies",
                         status = "primary",
                         "Small copepod abundance anomalies for the GOM region. Abundance anomalies estimated by averaging the individual abundance anomalies of Pseudocalanus spp., 
                                          Centropages hamatus, Centropages typicus, and Temora longicornis. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "GSI",
                         status = "primary",
                         "Annual time series of the Gulf Stream Index. Positive values are a more northerly Gulf Stream, and Negative values are a more southerly Gulf Stream. Anomalies of latitudinal position. 1954-present.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Hudson River Flow Rate (cubic meters per second)",
                         status = "primary",
                         "Mean annual flow of the Hudson River in cubic meters per second at the USGS gauge 01358000 at Green Island, New York.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "NAO",
                         status = "primary",
                         "North Atlantic Oscillation (NAO). Unit-less. 1864-present. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       #                       accordionItem(
                       #                         title = "OISST Anomaly (Season)",
                       #                         status = "primary",
                       #                         "SST anomalies for the GOM region for either the winter, spring, summer, or fall season. These data were derived from the NOAA Optimum Interpolation SST High Resolution data set (NOAA OISST V2). 
                       #                                          The 1982-2020 climatology was used to calculate anomalies.1982-present. In ℃.  Data are sourced from the R package 'ecodata'. 
                       #                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                       #                       ),
                       accordionItem(
                         title = "In-situ SST Anomaly (NEFSC)",
                         status = "primary",
                         "Annual SST anomalies for the GOM region. SST data collected from NEFSC survey from 1977-present. 
                                          In ℃. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Sea Surface Salinity",
                         status = "primary",
                         "Sea surface salinity (so[10^-3]) data are sourced from the Global Ocean Physics Reanalysis Product 'GLOBAL_MULTIYEAR_PHY_001_030'. These data are
               available from 1993-2020 and the most surface water layer (-0.5m) was selected. Salinity data processing methods for this project can be found in 'salinity_GLORYs_1993-2020.R'"
                       )
                       
                     ),#accordion
                     title = "Environmental and Climate Data", footer = NULL, status = "success",
                     solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                     collapsible = TRUE, collapsed = TRUE)#box
                   ,
                   
                   box(  
                     #####Striped Bass accordion#####
                     accordion(
                       id = "accordion2",
                       accordionItem(
                         title = "Striped Bass (Morone saxatilis):",
                         status = "success",
                         collapsed = TRUE,
                         accordionItem(
                           title = "Commercial Landings (mt):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass commercial landings in metric tons. 1947-2017. Data were sourced from the 2018 Benchmark Stock Assessment (SAW 66)"
                         ),
                         accordionItem(
                           title = "Female SSB (mt):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass female spawning stock biomass in metric tons. 1982-2021. Data were sourced from the 2021 Striped Bass assessment update"
                         ),
                         accordionItem(
                           title = "Fishing Mortality Rate (Full F):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass Fishing Mortality. 1982-2021. Data were sourced from the 2021 Striped Bass assessment update"
                         ),
                         accordionItem(
                           title = "Recreational Landings (mt):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass recreational landings in metric tons. 1982-2017. Data were sourced from the 2018 Benchmark Stock Assessment (SAW 66)"
                         ),
                         accordionItem(
                           title = "Maine Recreational Harvest (numbers of fish/Year):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass annual recreational landings for the state of Maine (numbers of fish). 1982-2017. Data were sourced from the 2018 Benchmark Stock Assessment (SAW 66)"
                         ),
                         accordionItem(
                           title = "Age 1 Population Abundance:",
                           status = "primary",
                           collapsed = FALSE,
                           "Estimates of age-specific (age 1) population abundance, 1982-2021. These data were sourced from the 2022 Atlantic Striped Bass Stock Assessment Update Appendices."
                         ),
                         accordionItem(
                           title = "Total Population Abundance:",
                           status = "primary",
                           collapsed = FALSE,
                           "Estimates of age-specific (ages 1-15+) population abundance, 1982-2021. These data were sourced from the 2022 Atlantic Striped Bass Stock Assessment Update Appendices."
                         ),
                         accordionItem(
                           title = "Weight at Age 1 (kg):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass mean weight at age 1 (WAA) in kilograms. 1982-2017. Data were sourced from the 2018 Benchmark Stock Assessment (SAW 66)"
                         ),
                         accordionItem(
                           title = "Weight at Age 4 (kg):",
                           status = "primary",
                           collapsed = FALSE,
                           "Striped Bass mean weight at age 4 (WAA) in kilograms. 1982-2017. Data were sourced from the 2018 Benchmark Stock Assessment (SAW 66)"
                         )
                       )
                     ),
                     #####Bluefin Tuna accordion#####
                     accordion(
                       id = "accordion3",
                       accordionItem(
                         title = "Atlantic Bluefin Tuna (Thunnus thynnus):",
                         status = "success",
                         collapsed = TRUE,
                         accordionItem(
                           title = "Bluefin Tuna Spawning Stock Biomass (mt)",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic Bluefin Tuna Spawning Stock Biomass (SSB). Units are in metric tons (mt). Data are sourced from the 2021 Atlantic Bluefin Tuna stock assessment. These SSB data were derived from the Stock Synthesis assessment model."
                         ),
                         accordionItem(
                           title = "Bluefin Tuna Recruitment",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic Bluefin Tuna Recruitment (abundance). Data represent the number of age 0 fish that survive to the end of their first year of life. Data are sourced from the 2021 Atlantic Bluefin Tuna stock assessment. These data were derived from the Stock Synthesis assessment model."
                         ),
                         accordionItem(
                           title = "Bluefin Tuna Fishing Mortality",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic Bluefin Fishing mortality (instantaneous F). Data represent the instantaneous fishing mortality for Bluefin Tuna ages 10-20. Data are sourced from the 2021 Atlantic Bluefin Tuna stock assessment. These data were derived from the Stock Synthesis assessment model."
                         )
                       )
                     ),
                     #####American Lobster accordion#####
                     accordion(
                       id = "accordion3",
                       accordionItem(
                         title = "American Lobster (Homarus americanus):",
                         status = "success",
                         collapsed = TRUE,
                         accordionItem(
                           title = "American Lobster Seasonal Latitude and Depth",
                           status = "primary",
                           collapsed = FALSE,
                           "American Lobster Seasonal Latitude (Decimal Degrees) and Depth (m) are sourced from the NOAA Fisheries' Distribution Mapping and Analysis Portal (DisMAP). Data represent the mean center of gravity (geographic center) metrics from the NEFSC bottom trawl survey. Metrics were calculated as biomass-weighted averages of depth and latitude, weighted by the interpolated biomass at each depth or latitude for each year and season (fall, spring) of the bottom trawl survey.  See https://apps-st.fisheries.noaa.gov/dismap/ for more info. "
                         )
                       )
                     ),
                     #####American Plaice accordion#####
                     accordion(
                       id = "accordion4",
                       accordionItem(
                         title = "American Plaice (Hippoglossoides platessoides):",
                         status = "success",
                         collapsed = TRUE,
                         accordionItem(
                           title = "American Plaice Seasonal Latitude and Depth",
                           status = "primary",
                           collapsed = FALSE,
                           "American Plaice Seasonal Latitude (Decimal Degrees) and Depth (m) are sourced from the NOAA Fisheries' Distribution Mapping and Analysis Portal (DisMAP). Data represent the mean center of gravity (geographic center) metrics from the NEFSC bottom trawl survey. Metrics were calculated as biomass-weighted averages of depth and latitude, weighted by the interpolated biomass at each depth or latitude for each year and season (fall, spring) of the bottom trawl survey.  See https://apps-st.fisheries.noaa.gov/dismap/ for more info. "
                         ),
                         accordionItem(
                           title = "American Plaice Spawning Stock Biomass (mt)",
                           status = "primary",
                           collapsed = FALSE,
                           "American Plaice Spawning Stock Biomass (SSB). Units are in metric tons (mt). Data are from the Spring and Fall Northeast Fisheries Science Center (NEFSC) bottom trawl survey observations. Aggregate biomass indices were estimated in Albatross IV calibrated (Miller et al. 2010) units (kg/tow) across the timeseries."
                         ),
                         accordionItem(
                           title = "American Plaice Age 1 Mean Catch per Tow (numbers/tow)",
                           status = "primary",
                           collapsed = FALSE,
                           "Standardized stratified mean number per tow of age 1 American plaice in the NEFSC spring and fall research bottom trawl survey in the Gulf of Maine and Georges Bank area (offshore strata 13-30, 36-40). Sourced from Tables 9a and 9b of the 2019 American Plaice Research Track Stock Assessment Update Report."
                         ),
                         accordionItem(
                           title = "American Plaice Stock Numbers (thousands of age 1 fish)",
                           status = "primary",
                           collapsed = FALSE,
                           "Estimates of beginning year stock size (thousands of fish) of Age 1 Gulf of Maine-Georges Bank American plaice, estimated from virtual population analysis (VPA), calibrated using the commercial catch at age ADAPT formulation. Sourced from Table 11 of the 2019 American Plaice Research Track Stock Assessment Update Report."
                         ),
                         accordionItem(
                           title = "American Plaice Mean Relative Condition",
                           status = "primary",
                           collapsed = FALSE,
                           " Relative condition index data were calculated as the ratio of observed weight to predicted weight at a given length from the fall NEFSC trawl survey from 1992-2019 (NEFSC 2022). These data included American plaice relative condition indices for the Gulf of Maine region and are filtered to only include years with at least 3 samples."
                         ),
                       )
                     ),
                     #############
                     title = "Stock Data", footer = NULL, status = "success",
                     solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                     collapsible = TRUE, collapsed = TRUE)#box
)#tabitem