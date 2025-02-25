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
                         title = "Annual Atlantic Multidecadal Oscillation (AMO):",
                         status = "primary",
                         "Atlantic Multidecadal Oscillation (AMO) Index. These data represent annual means of the NOAA Physical Sciences Laboratory's unsmoothed short monthly AMO dataset found at https://psl.noaa.gov/data/timeseries/AMO/. 1948-present. These data represent the weighted average over the N Atlantic from 0 to 70N, and have been detrended."
                       ),
                       accordionItem(
                         title = "Atlantic Herring Seasonal Latitude and Depth (Prey)",
                         status = "primary",
                         "Atlantic Herring Seasonal Latitude (Decimal Degrees) and Depth (m) are sourced from the NOAA Fisheries' Distribution Mapping and Analysis Portal (DisMAP). Data represent the mean center of gravity (geographic center) metrics from the NEFSC bottom trawl survey. Metrics were calculated as biomass-weighted averages of depth and latitude, weighted by the interpolated biomass at each depth or latitude for each year and season (fall, spring) of the bottom trawl survey.  See https://apps-st.fisheries.noaa.gov/dismap/ for more info. "
                       ),
                       accordionItem(
                         title = "Bottom Temperature Absolute",
                         status = "primary",
                         "Annual bottom temperatures for the Gulf of Maine (GOM) region. Bottom Temperature data collected from NEFSC survey from 1977-present. 
                                        In ℃.Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
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
                         title = "Bottom Temperature Anomaly (for Atlantic cod page)",
                         status = "primary",
                         "Atlantic cod Bottom water temperature data were sourced from the high-resolution, long-term bottom temperature product for the Northeast U.S. continental shelf, as described in du Pontavice et al. (2023). 
                         To characterize the bottom temperature environment fish were experiencing prior to capture, means over the six months prior to the start of each seasonal NEFSC Bottom Trawl survey were used. Temperature anomalies were calculated for the years 1982-2019, using 1982-2011 as a reference baseline period for comparison."
                       ),
                       accordionItem(
                         title = "Calanus Abundance Anomaly",
                         status = "primary",
                         "For species other than Atlantic cod, Calanus finmarchicus abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info. "
                       ),
                       accordionItem(
                         title = "Calanus finmarchicus and Pseudocalanus spp. Abundance Anomalies /100m^3",
                         status = "primary",
                         "For the Atlantic cod data specifically, Zooplankton abundance data were sourced from the NOAA Ecosystem Monitoring (EcoMon) program. 
                         For the EGOM & WGOM stock area models, summer zooplankton survey months (June-August) were used, and in GBK and SNE stock area models, 
                         spring zooplankton survey months (March-May) were used as these time periods align with the start of, or begin just after, the peak 
                         spawning period for cod, when cod larvae would likely be feeding on zooplankton (Kane 1984; Heath and Lough 2007; Jacobsen et al. 2020). 
                         All zooplankton data were lagged one year and were subsetted to match cod stock areas."
                       ),
                       accordionItem(
                         title = "Cumulative Heatwave Index",
                         status = "primary",
                         "For the Atlantic cod species page, annual cumulative marine heatwave data were provided by the Northeast Fisheries Science Center via ecodata. 
                         The marine heatwave dataset was masked to the Atlantic cod research track working group stock regions. 
                         This dataset can be found under “ESP_heatwave_cod” in the ecodata R. See https://noaa-edab.github.io/tech-doc/ for more info."
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
                         title = "Gulf Stream Index (GSI)",
                         status = "primary",
                         "Annual time series of the Gulf Stream Index (GSI). Positive values are a more northerly Gulf Stream, and Negative values are a more southerly Gulf Stream. Anomalies of latitudinal position. 1954-present.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "Hudson River Flow Rate (cubic meters per second)",
                         status = "primary",
                         "Mean annual flow of the Hudson River in cubic meters per second at the USGS gauge 01358000 at Green Island, New York.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                       accordionItem(
                         title = "North Atlantic Oscillation (NAO)",
                         status = "primary",
                         "North Atlantic Oscillation (NAO). Unit-less. 1864-present. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                       ),
                      accordionItem(
                         title = "Sea Surface Temperature Anomaly (for Atlantic cod page)",
                         status = "primary",
                         "Sea surface temperature (SST) data used in the Atlantic cod page were sourced from the National Oceanic and Atmospheric Administration (NOAA) Physical Sciences Laboratory. 
                         Optimum Interpolation Sea Surface Temperature (NOAA OISST V2) data were used, a long-term record of climate data that utilizes multiple data collection platforms into a global grid. 
                         Data were masked to cod stock regions and a monthly spatial average was calculated for each stock area. SST data were averaged over a four-month 
                         recruitment period, which was temporally aligned with the beginning of the peak spawning period of the previous year for each stock area. 
                         4-month means were chosen as cod eggs are buoyant and range between 90-150 days in settlement timing (McBride and Smedbol 2022). 
                         Years 1982-2011 of the corresponding 4-month time periods were used as the reference base period to calculate the final SST anomaly datasets for each season.
                         In ℃."
                         ),
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
                       id = "accordion4",
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
                       id = "accordion5",
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
                     #####Atlantic Cod accordion#####
                     accordion(
                       id = "accordion6",
                       accordionItem(
                         title = "Atlantic cod (Gadus morhua):",
                         status = "success",
                         collapsed = TRUE,
                         accordionItem(
                           title = "Atlantic cod Seasonal Latitude and Depth",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic cod Seasonal Latitude (Decimal Degrees) and Depth (m) are sourced from the NOAA Fisheries' Distribution Mapping 
                           Analysis Portal (DisMAP). Data represent the mean center of gravity (geographic center) metrics from the NEFSC bottom trawl 
                           survey. Metrics were calculated as biomass-weighted averages of depth and latitude, weighted by the interpolated biomass at 
                           each depth or latitude for each year and season (fall, spring) of the bottom trawl survey.  
                           See https://apps-st.fisheries.noaa.gov/dismap/ for more info. "
                         ),
                         accordionItem(
                           title = "Atlantic cod Spawning Stock Biomass (SSB; mt)",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic cod Spawning Stock Biomass (SSB). Units are in metric tons (mt).
                           Spawning stock biomass (SSB) data were estimated using spring and fall NEFSC bottom trawl survey numbers at age and weights at age for cod ages 4+. 
                           The aggregate biomass for these indices was calculated for each year (1982-2019) and for each season, in units of kg/tow."
                         ),
                         accordionItem(
                           title = "Atlantic cod Recruits per Spawning Stock Biomass (R/SSB)",
                           status = "primary",
                           collapsed = FALSE,
                           "Annual abundance of age 1 fish from the NEFSC trawl survey were used for recruitment data (see Atlantic cod Age 1 Abundance).
                           See Atlantic cod Spawning Stock Biomass for more information on the SSB data used. Recruits per spawner was used here as a metric of recruitment success. 
                           These data were calculated as recruitment = index of abundance at age 1 in year t per SSB in year t-1 for each season and year, e.g., Rt/SSBt-1. "
                         ),
                         accordionItem(
                           title = "Atlantic cod Age 1 Abundance (numbers/tow)",
                           status = "primary",
                           collapsed = FALSE,
                           "Standardized stratified mean number per tow of age 1 Atlantic cod in the NEFSC spring and fall research bottom trawl survey in each stock area (WGOM, EGOM, GBK, and SNE)."
                         ),
                         accordionItem(
                           title = "Atlantic cod Mean Relative Condition",
                           status = "primary",
                           collapsed = FALSE,
                           " Relative condition index data were calculated as the ratio of observed weight to predicted weight at a given length from the 
                           fall NEFSC trawl survey from 1992-2019 (NEFSC 2022). These data included Atlantic cod relative condition indices for each stock are and are filtered to only include years with at least 3 samples."
                         ),
                         accordionItem(
                           title = "Atlantic cod Weight at Age (WAA) for ages 1 or 6",
                           status = "primary",
                           collapsed = FALSE,
                           "Atlantic cod WAA anomalies were calculated from the NEFSC Bottom Trawl survey for ages 1 and 6 from 1982-2019. Ages 1 and 6
                           were used as Age 1 is useful for seeing trends in recruited fish to the fishery, while age 6 cod are considered to be 100% 
                           fully mature by that age, and is thus useful for looking at WAA trends of mature fish.
                           Weight at age data were limited, especially for SNE and EGOM stocks. Only stocks and ages which had >30 years of 
                           data were used and 1982-2011 base-periods (or as close to that range as possible while still maintaining 30 years for a base 
                           period) were used to calculate the means for the anomaly calculations. Data was included as available, so not every stock area or season may have both ages available."
                         ),
                       )
                     ),
                     #############
                     title = "Stock Data", footer = NULL, status = "success",
                     solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                     collapsible = TRUE, collapsed = TRUE)#box
)#tabitem