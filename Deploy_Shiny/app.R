#### Load R packages ######
library(DT)
library(shiny)
library(here)
library(htmltools)
library(shinydashboardPlus)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(utils)
library(Rcpp)
library(cli)
library(graphics)
library(tidyr)
#here()
#setwd("C:/Users/jamie.behan/Documents/Projects/Indicator_Visualizations/Indicator_Visualizations2/Deploy_Shiny")
#print(getwd())
##### LOAD DATA ######
source("Compile_data.R")
##### LOAD supporting functions ================================================ ########
source("supporting_functions_for_app.R")
#######################################################################################
###### Define UI =============================================================== ######
ui <- dashboardPage(
  dashboardHeader(titleWidth = 150,headerlogos),   #logos that appear in the header. see "supporting_functions_for_app.R"
  sidebar, #sidebar. see "supporting_functions_for_app.R"
  dashboardBody(gmri_colors,
    fluidPage(tweaks,
      tabItems(
        Hometab,
        How_To,
        tabItem(     #Stripedbass tab
          tabName = "StripedBass",
          h2(img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/2022-08/640x427-StripedBass-NOAAFisheries.png?itok=4ZQoQM0S",
                 style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
             HTML('<strong style="font-size: 40px;">Atlantic Striped Bass</strong> (<em>Morone saxatilis</em>)')),
          mainPanel(width = 12,
                    tabsetPanel(type = "tabs",
                                stripedbass_info, #text that shows up on info page for stripedbass tab. code located in "supporting_functions_for_app.R"
                                tabPanel(
                                  "Interactive Plots",
                                  h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      h3("Stock Variables", style = "font-weight: bold;"),
                                      h4("Variables related to recruitment", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Striped_Bass_Age1_Abundance", "Striped_Bass_Female_SSB"),var_name="SB_recruitment_variable")),
                                      h4("Variables related to growth", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Striped_Bass_Weight_at_Age_1_kg", "Striped_Bass_Weight_at_Age_4_kg"),var_name="SB_growth_variable")),
                                      h4("Other variables", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Striped_Bass_Total_Abundance", "Striped_Bass_Commercial_Landings",
                                                                                  "Striped_Bass_Recreational_Landings", "Striped_Bass_Maine_Recreational_Harvest", "Striped_Bass_Full_F"),var_name="SB_other_variable")),
                                      radioButtons(
                                        "SB_Plotting_Style",
                                        "Select Plotting Style",
                                        choices = c("Layered (Choose up to 5 Variables)" = "Layered", "Stacked" = "Stacked"),
                                        selected = "Stacked"
                                      ),
                                      prettySwitch(
                                        inputId = "trendline_SB",
                                        label = "Show Trendline(s)",
                                        fill = TRUE, 
                                        status = "primary"
                                      ),
                                      prettySwitch(
                                        inputId = "lta_SB",
                                        label = "Show Long Term Average Line",
                                        fill = TRUE, 
                                        status = "primary"
                                      )),
                                    column(
                                      width = 6,
                                      h3("Environmental Variables", style = "font-weight: bold;"),
                                      h4("Abiotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Atlantic_Multidecadal_Oscillation_Annual_Mean","Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM",
                                                                                  "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM","Gulf_Stream_Index_ALL","Hudson_River_Flow_Rate_MAB",
                                                                                  "North_Atlantic_Oscillation_ALL","Surface_Salinity_Mean"),var_name="SB_Abiotic_variable")),
                                      h4("Biotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Annual_Forage_Fish_Biomass_GOM",
                                                                                  "Atlantic_herring_Fall_Depth_COG","Atlantic_herring_Fall_Lat_COG","Atlantic_herring_Spring_Depth_COG",   
                                                                                  "Atlantic_herring_Spring_Lat_COG","Calfin_anomaly_GOM","Large_Copepods_Abundance_Anomaly_GOM",
                                                                                  "Small_Copepods_Abundance_Anomaly_GOM"),var_name="SB_Biotic_variable"))
                                    )),
                                  mainPanel(
                                    width = 12,
                                    plotlyOutput("stripedbass_plot",
                                                 height = "650px")
                                  )),
                                tabPanel("Data", 
                                  mainPanel(width = 12,
                                            DT::dataTableOutput("mytable_SB"),
                                            downloadButton("downloadCSV_SB", "Download .CSV"))
                                )

                                ))), #close tabitem
        tabItem(
          tabName = "BluefinTuna",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/2022-09/640x427-Tuna-Bluefin-NOAAFisheries.png?itok=G5iVOYPt", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">Atlantic Bluefin Tuna</strong> (<em>Thunnus thynnus</em>)')),
          mainPanel(width = 12,
                    tabsetPanel(type = "tabs",
                                BFT_info, #text that shows up on info page for bluefin tuna tab. code located in "supporting_functions_for_app.R"
                                tabPanel(
                                  "Interactive Plots",
                                  h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      h3("Stock Variables", style = "font-weight: bold;"),
                                      h4("Variables related to recruitment", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Bluefin_Recruitment", "Bluefin_SSB_mt"),var_name="BFT_recruitment_variable")),
                                      h4("Variables related to growth", style = "font-weight: bold;"),
                                      #div(class = "fish-controls",fish_controls(c("Striped_Bass_Weight_at_Age_1_kg", "Striped_Bass_Weight_at_Age_4_kg"),var_name="BFT_growth_variable")),
                                      h4("Other variables", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Bluefin_F"),var_name="BFT_other_variable")),
                                      radioButtons(
                                        "BFT_Plotting_Style",
                                        "Select Plotting Style",
                                        choices = c("Layered (Choose up to 5 Variables)" = "Layered", "Stacked" = "Stacked"),
                                        selected = "Stacked"
                                      ),
                                      prettySwitch(
                                        inputId = "trendline_BFT",
                                        label = "Show Trendline(s)",
                                        fill = TRUE, 
                                        status = "primary"
                                      ),
                                    prettySwitch(
                                      inputId = "lta_BFT",
                                      label = "Show Long Term Average Line",
                                      fill = TRUE, 
                                      status = "primary"
                                    )),
                                    column(
                                      width = 6,
                                      h3("Environmental Variables", style = "font-weight: bold;"),
                                      h4("Abiotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Atlantic_Multidecadal_Oscillation_Annual_Mean","Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM",
                                                                                  "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM","Gulf_Stream_Index_ALL",
                                                                                  "North_Atlantic_Oscillation_ALL","Surface_Salinity_Mean"),var_name="BFT_Abiotic_variable")),
                                      h4("Biotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Annual_Forage_Fish_Biomass_GOM",
                                                                                  "Atlantic_herring_Fall_Depth_COG","Atlantic_herring_Fall_Lat_COG","Atlantic_herring_Spring_Depth_COG",   
                                                                                  "Atlantic_herring_Spring_Lat_COG","Calfin_anomaly_GOM","Large_Copepods_Abundance_Anomaly_GOM",
                                                                                  "Small_Copepods_Abundance_Anomaly_GOM"),var_name="BFT_Biotic_variable"))
                                    )),
                                  mainPanel(
                                    width = 12,
                                    plotlyOutput("BFT_plot",
                                                 height = "650px")
                                  )),
                                tabPanel("Data", 
                                         mainPanel(width = 12,
                                                   DT::dataTableOutput("mytable_BFT"),
                                                   downloadButton("downloadCSV_BFT", "Download .CSV"))
                                )
                                
                    ))), #close tabitem
        tabItem(
          tabName = "AmericanLobster",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/dam-migration/640x427-american-lobster.png?itok=FX0oMipE", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">American Lobster</strong> (<em>Homarus americanus</em>)')),
            mainPanel(width = 12,
                      tabsetPanel(type = "tabs",
                                  AL_info, #text that shows up on info page for lobster tab. code located in "supporting_functions_for_app.R"
                                  tabPanel(
                                    "Interactive Plots",
                                    h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                                    fluidRow(
                                      column(
                                        width = 6,
                                        h3("Stock Variables", style = "font-weight: bold;"),
                                        h4("Variables related to recruitment", style = "font-weight: bold;"),
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_Age1_Abundance", "Striped_Bass_Female_SSB"),var_name="AL_recruitment_variable")),
                                        h4("Variables related to Distribution", style = "font-weight: bold;"),
                                        div(class = "fish-controls",fish_controls(c("American_lobster_Fall_Lat_COG","American_lobster_Spring_Lat_COG","American_lobster_Fall_Depth_COG","American_lobster_Spring_Depth_COG"),var_name="AL_distribution_variable")),
                                        h4("Other variables", style = "font-weight: bold;"),
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_Total_Abundance", "Striped_Bass_Commercial_Landings",
                                        #                                            "Striped_Bass_Recreational_Landings", "Striped_Bass_Maine_Recreational_Harvest", "Striped_Bass_Full_F"),var_name="AL_other_variable")),
                                        radioButtons(
                                          "AL_Plotting_Style",
                                          "Select Plotting Style",
                                          choices = c("Layered (Choose up to 5 Variables)" = "Layered", "Stacked" = "Stacked"),
                                          selected = "Stacked"
                                        ),
                                        prettySwitch(
                                          inputId = "trendline_AL",
                                          label = "Show Trendline(s)",
                                          fill = TRUE, 
                                          status = "primary"
                                        ),
                                      prettySwitch(
                                        inputId = "lta_AL",
                                        label = "Show Long Term Average Line",
                                        fill = TRUE, 
                                        status = "primary"
                                      )),
                                      column(
                                        width = 6,
                                        h3("Environmental Variables", style = "font-weight: bold;"),
                                        h4("Abiotic", style = "font-weight: bold;"),
                                        div(class = "fish-controls",fish_controls(c("Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM",
                                                                                    "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM"),var_name="AL_Abiotic_variable")),
                                        h4("Biotic", style = "font-weight: bold;"),
                                        div(class = "fish-controls",fish_controls(c("Annual_Forage_Fish_Biomass_GOM",
                                                                                    "Atlantic_herring_Fall_Depth_COG","Atlantic_herring_Fall_Lat_COG","Atlantic_herring_Spring_Depth_COG",   
                                                                                    "Atlantic_herring_Spring_Lat_COG"),var_name="AL_Biotic_variable"))
                                      )),
                                    mainPanel(
                                      width = 12,
                                      plotlyOutput("AL_plot",
                                                   height = "650px")
                                    )),
                                  tabPanel("Data", 
                                           mainPanel(width = 12,
                                                     DT::dataTableOutput("mytable_AL"),
                                           downloadButton("downloadCSV_AL", "Download .CSV"))
                                  )
                                  
                      ))), #close tabitem
        tabItem(
          tabName = "AmericanPlaice",
          h2(
            img(src = "https://i.ibb.co/pnqcby9/american-plaice-oil1removebg.png", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">American Plaice</strong> (<em>Hippoglossoides platessoides</em>)')),
          mainPanel(width = 12,
                    tabsetPanel(type = "tabs",
                                AP_info, #text that shows up on info page for plaice tab. code located in "supporting_functions_for_app.R"
                                tabPanel(
                                  "Interactive Plots",
                                  h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      h3("Stock Variables", style = "font-weight: bold;"),
                                      h4("Variables related to recruitment", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Plaice_Age_1_Stock_Size", "Plaice_Spring_Age_1_Numbers_per_Tow","Plaice_Fall_Age_1_Numbers_per_Tow","Plaice_SSB_Spring","Plaice_SSB_Fall"),var_name="AP_recruitment_variable")),
                                      h4("Variables related to Distribution", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Plaice_Depth_Spring","Plaice_Lat_Spring","Plaice_Depth_Fall","Plaice_Lat_Fall"),var_name="AP_distribution_variable")),
                                      h4("Other variables", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Plaice_Mean_Condition_Fall"),var_name="AP_other_variable")),
                                      radioButtons(
                                        "AP_Plotting_Style",
                                        "Select Plotting Style",
                                        choices = c("Layered (Choose up to 5 Variables)" = "Layered", "Stacked" = "Stacked"),
                                        selected = "Stacked"
                                      ),
                                      prettySwitch(
                                        inputId = "trendline_AP",
                                        label = "Show Trendline(s)",
                                        fill = TRUE, 
                                        status = "primary"
                                      ),
                                    prettySwitch(
                                      inputId = "lta_AP",
                                      label = "Show Long Term Average Line",
                                      fill = TRUE, 
                                      status = "primary"
                                    )),
                                    column(
                                      width = 6,
                                      h3("Environmental Variables", style = "font-weight: bold;"),
                                      h4("Abiotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Atlantic_Multidecadal_Oscillation_Annual_Mean","Annual_Bottom_Temp_Absolute_GOM","Gulf_Stream_Index_ALL","North_Atlantic_Oscillation_ALL","SST_Temp_Anomaly_GOM",
                                                                                  "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM"),var_name="AP_Abiotic_variable")),
                                    )),
                                  mainPanel(
                                    width = 12,
                                    plotlyOutput("AP_plot",
                                                 height = "650px")
                                  )),
                                tabPanel("Data", 
                                         mainPanel(width = 12,
                                                   DT::dataTableOutput("mytable_AP"),
                                                   downloadButton("downloadCSV_AP", "Download .CSV"))
                                )
                                
                    ))), #close tabitem
        tabItem(
          tabName = "AtlanticCod",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3//2022-07/640x427-Cod-Atlantic-NOAAFisheries.png?itok=LaaW5W6M", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">Atlantic Cod</strong> (<em>Gadus morhua</em>)')),
          mainPanel(width = 12,
                    tabsetPanel(type = "tabs",
                                AC_info, #text that shows up on info page for cod tab. code located in "supporting_functions_for_app.R"
                                tabPanel(
                                  "Interactive Plots",
                                  h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                                  selectInput(
                                    "AC_stock_area",
                                    "Select Stock Area",
                                    choices = c("Western Gulf of Maine" = "WGOM",
                                                "Eastern Gulf of Maine" = "EGOM",
                                                "Georges Bank" = "GBK",
                                                "Southern New England" = "SNE"),
                                    selected = "WGOM"
                                  ),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      h3("Stock Variables", style = "font-weight: bold;"),
                                      h4("Variables related to recruitment", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Age.1_WGOM_Spring","Age.1_WGOM_Fall","Age.1_EGOM_Spring","Age.1_EGOM_Fall","Age.1_GBK_Spring","Age.1_GBK_Fall","Age.1_SNE_Spring","Age.1_SNE_Fall",
                                                                                  "SSB_WGOM_Spring","SSB_WGOM_Fall","SSB_EGOM_Spring","SSB_EGOM_Fall","SSB_GBK_Spring","SSB_GBK_Fall","SSB_SNE_Spring","SSB_SNE_Fall"),var_name="AC_recruitment_variable")),
                                      h4("Variables related to Distribution", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Cod_Depth_Spring","Cod_Lat_Spring","Cod_Depth_Fall","Cod_Lat_Fall"),var_name="AC_distribution_variable")),
                                      h4("Variables related to growth", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("K_rel_WGOM_Spring","K_rel_WGOM_Fall","K_rel_EGOM_Spring","K_rel_EGOM_Fall","K_rel_GBK_Spring","K_rel_GBK_Fall","K_rel_SNE_Spring","K_rel_SNE_Fall",
                                                                                  "Age.1_EGOM_Fall","Age.1_EGOM_Spring","Age.1_GBK_Fall","Age.1_GBK_Spring","Age.1_SNE_Fall","Age.1_SNE_Spring","Age.1_WGOM_Fall","Age.1_WGOM_Spring"),var_name="AC_growth_variable")),
                                      radioButtons(
                                        "AC_Plotting_Style",
                                        "Select Plotting Style",
                                        choices = c("Layered (Choose up to 5 Variables)" = "Layered", "Stacked" = "Stacked"),
                                        selected = "Stacked"
                                      ),
                                      prettySwitch(
                                        inputId = "trendline_AC",
                                        label = "Show Trendline(s)",
                                        fill = TRUE, 
                                        status = "primary"
                                      ),
                                      prettySwitch(
                                        inputId = "lta_AC",
                                        label = "Show Long Term Average Line",
                                        fill = TRUE, 
                                        status = "primary"
                                      )),
                                    column(
                                      width = 6,
                                      h3("Environmental Variables", style = "font-weight: bold;"),
                                      h4("Abiotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM","Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM",
                                                                                  "bt_anomaly_EGOM_Fall","bt_anomaly_EGOM_Spring","bt_anomaly_GBK_Fall","bt_anomaly_GBK_Spring","bt_anomaly_SNE_Fall","bt_anomaly_SNE_Spring","bt_anomaly_WGOM_Fall","bt_anomaly_WGOM_Spring",
                                                                                  "GSI_EGOM_Fall","GSI_EGOM_Spring","GSI_GBK_Fall","GSI_GBK_Spring","GSI_SNE_Fall","GSI_SNE_Spring","GSI_WGOM_Fall","GSI_WGOM_Spring" ,        
                                                                                  "Heatwave_EGOM_Fall","Heatwave_EGOM_Spring","Heatwave_GBK_Fall","Heatwave_GBK_Spring","Heatwave_SNE_Fall","Heatwave_SNE_Spring","Heatwave_WGOM_Fall","Heatwave_WGOM_Spring",
                                                                                  "sst_anomaly_EGOM_Fall","sst_anomaly_EGOM_Spring","sst_anomaly_GBK_Fall","sst_anomaly_GBK_Spring","sst_anomaly_SNE_Fall","sst_anomaly_SNE_Spring","sst_anomaly_WGOM_Fall","sst_anomaly_WGOM_Spring"),var_name="AC_Abiotic_variable")),
                                      h4("Biotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("calfin_100m3_EGOM_Fall","calfin_100m3_EGOM_Spring","calfin_100m3_GBK_Fall","calfin_100m3_GBK_Spring","calfin_100m3_SNE_Fall","calfin_100m3_SNE_Spring","calfin_100m3_WGOM_Fall","calfin_100m3_WGOM_Spring",
                                                                                  "pseudo_100m3_EGOM_Fall","pseudo_100m3_EGOM_Spring","pseudo_100m3_GBK_Fall","pseudo_100m3_GBK_Spring","pseudo_100m3_SNE_Fall","pseudo_100m3_SNE_Spring","pseudo_100m3_WGOM_Fall","pseudo_100m3_WGOM_Spring"),var_name="AC_Biotic_variable"))
                                    )),
                                  mainPanel(
                                    width = 12,
                                    plotlyOutput("AC_plot",
                                                 height = "650px")
                                  )),
                                tabPanel("Data", 
                                         mainPanel(width = 12,
                                                   DT::dataTableOutput("mytable_AC"),
                                                   downloadButton("downloadCSV_AC", "Download .CSV"))
                                )
                                
                    ))), #close tabitem
###### Source metadata code ==================================================== ####
source("app_metadata.R", local = TRUE)$value
###### close metadata section ================================================= #####
      )#close tabItems
    )#close fluidpage
  )#close dashboard body
)#close dashboardpage
###### Define server function  ######
server <- function(input, output, session) {
  
  dataDf <- reactive({
    temp <- Both2
  })
  #New part of server making reactive checkboxes specifically for cod page #
  observe({
    selected_area <- input$AC_stock_area
    
    # Filter recruitment variables
    recruitment_vars <- c("Age.1_WGOM_Spring","Age.1_WGOM_Fall","Age.1_EGOM_Spring","Age.1_EGOM_Fall",
                          "Age.1_GBK_Spring","Age.1_GBK_Fall","Age.1_SNE_Spring","Age.1_SNE_Fall",
                          "SSB_WGOM_Spring","SSB_WGOM_Fall","SSB_EGOM_Spring","SSB_EGOM_Fall",
                          "SSB_GBK_Spring","SSB_GBK_Fall","SSB_SNE_Spring","SSB_SNE_Fall")
    filtered_recruitment <- recruitment_vars[grep(selected_area, recruitment_vars)]
    updateCheckboxGroupInput(session, "AC_recruitment_variable", choices = filtered_recruitment)
    
    # Filter distribution variables
    distribution_vars <- c("Cod_Depth_Spring","Cod_Lat_Spring","Cod_Depth_Fall","Cod_Lat_Fall")
    # Note: Distribution vars don't have area in their names, so we'll keep all of them
    updateCheckboxGroupInput(session, "AC_distribution_variable", choices = distribution_vars)
    
    # Filter growth variables
    growth_vars <- c("K_rel_WGOM_Spring","K_rel_WGOM_Fall","K_rel_EGOM_Spring","K_rel_EGOM_Fall",
                     "K_rel_GBK_Spring","K_rel_GBK_Fall","K_rel_SNE_Spring","K_rel_SNE_Fall",
                     "Age.1_EGOM_Fall","Age.1_EGOM_Spring","Age.1_GBK_Fall","Age.1_GBK_Spring",
                     "Age.1_SNE_Fall","Age.1_SNE_Spring","Age.1_WGOM_Fall","Age.1_WGOM_Spring")
    filtered_growth <- growth_vars[grep(selected_area, growth_vars)]
    updateCheckboxGroupInput(session, "AC_growth_variable", choices = filtered_growth)
    
    # Filter abiotic variables
    abiotic_vars <- c("bt_anomaly_EGOM_Fall","bt_anomaly_EGOM_Spring","bt_anomaly_GBK_Fall",
                      "bt_anomaly_GBK_Spring","bt_anomaly_SNE_Fall","bt_anomaly_SNE_Spring",
                      "bt_anomaly_WGOM_Fall","bt_anomaly_WGOM_Spring",
                      "GSI_EGOM_Fall","GSI_EGOM_Spring","GSI_GBK_Fall","GSI_GBK_Spring",
                      "GSI_SNE_Fall","GSI_SNE_Spring","GSI_WGOM_Fall","GSI_WGOM_Spring",
                      "Heatwave_EGOM_Fall","Heatwave_EGOM_Spring","Heatwave_GBK_Fall",
                      "Heatwave_GBK_Spring","Heatwave_SNE_Fall","Heatwave_SNE_Spring",
                      "Heatwave_WGOM_Fall","Heatwave_WGOM_Spring",
                      "sst_anomaly_EGOM_Fall","sst_anomaly_EGOM_Spring","sst_anomaly_GBK_Fall",
                      "sst_anomaly_GBK_Spring","sst_anomaly_SNE_Fall","sst_anomaly_SNE_Spring",
                      "sst_anomaly_WGOM_Fall","sst_anomaly_WGOM_Spring")
    filtered_abiotic <- abiotic_vars[grep(selected_area, abiotic_vars)]
    updateCheckboxGroupInput(session, "AC_Abiotic_variable", choices = filtered_abiotic)
    
    # Filter biotic variables
    biotic_vars <- c("calfin_100m3_EGOM_Fall","calfin_100m3_EGOM_Spring","calfin_100m3_GBK_Fall",
                     "calfin_100m3_GBK_Spring","calfin_100m3_SNE_Fall","calfin_100m3_SNE_Spring",
                     "calfin_100m3_WGOM_Fall","calfin_100m3_WGOM_Spring",
                     "pseudo_100m3_EGOM_Fall","pseudo_100m3_EGOM_Spring","pseudo_100m3_GBK_Fall",
                     "pseudo_100m3_GBK_Spring","pseudo_100m3_SNE_Fall","pseudo_100m3_SNE_Spring",
                     "pseudo_100m3_WGOM_Fall","pseudo_100m3_WGOM_Spring")
    filtered_biotic <- biotic_vars[grep(selected_area, biotic_vars)]
    updateCheckboxGroupInput(session, "AC_Biotic_variable", choices = filtered_biotic)
  })
  # End of new code for cod page #
  # Rest of Server Code for reactive plots #
  output$stripedbass_plot <- renderPlotly({
    num_variables <- length(c(input$SB_recruitment_variable, input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, input$SB_Biotic_variable))
    all_vars <- c(input$SB_recruitment_variable, input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, input$SB_Biotic_variable)
    plot_function(plottingstyle = input$SB_Plotting_Style, num_variables = num_variables, dataDf = dataDf, all_vars = all_vars, name_mapping = name_mapping, name_mapping2 = name_mapping2, show_trendline=input$trendline_SB,show_lta = input$lta_SB)

    })
  output$BFT_plot <- renderPlotly({
    num_variables <- length(c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable))
    all_vars<-c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable)
    plot_function(plottingstyle = input$BFT_Plotting_Style,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline=input$trendline_BFT,show_lta = input$lta_BFT)
  })#close renderPlotly
  output$AL_plot <- renderPlotly({
    num_variables <- length(c(input$AL_recruitment_variable,input$AL_distribution_variable,input$AL_other_variable, input$AL_Abiotic_variable,input$AL_Biotic_variable))
    all_vars<-c(input$AL_recruitment_variable,input$AL_distribution_variable,input$AL_other_variable, input$AL_Abiotic_variable,input$AL_Biotic_variable)
    plot_function(plottingstyle = input$AL_Plotting_Style,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline=input$trendline_AL,show_lta = input$lta_AL)
  })#close renderPlotly
  output$AP_plot <- renderPlotly({
    num_variables <- length(c(input$AP_recruitment_variable,input$AP_distribution_variable,input$AP_other_variable, input$AP_Abiotic_variable,input$AP_Biotic_variable))
    all_vars<-c(input$AP_recruitment_variable,input$AP_distribution_variable,input$AP_other_variable, input$AP_Abiotic_variable,input$AP_Biotic_variable)
    plot_function(plottingstyle = input$AP_Plotting_Style,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline=input$trendline_AP,show_lta = input$lta_AP)
  })#close renderPlotly
  output$AC_plot <- renderPlotly({
    num_variables <- length(c(input$AC_recruitment_variable, input$AC_distribution_variable, 
                              input$AC_growth_variable, input$AC_Abiotic_variable, 
                              input$AC_Biotic_variable))
    all_vars <- c(input$AC_recruitment_variable, input$AC_distribution_variable, 
                  input$AC_growth_variable, input$AC_Abiotic_variable, 
                  input$AC_Biotic_variable)
    
    if(length(all_vars) == 0) {
      return(
        plotly::plotly_empty() %>%
          add_annotations(
            text = "Error: Must select at least one variable to display plot(s)",
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(color = 'red', size = 14)
          ))}
  plot_function(plottingstyle = input$AC_Plotting_Style, 
                  num_variables = num_variables, 
                  dataDf = dataDf, 
                  all_vars = all_vars, 
                  name_mapping = name_mapping, 
                  name_mapping2 = name_mapping2, 
                  show_trendline = input$trendline_AC,
                  show_lta = input$lta_AC)
  })#close renderPlotly
  # Define reactive expressions for data subsets
  table_subset_SB <- reactive({
    all_varsY <- c("Year", input$SB_recruitment_variable, input$SB_distribution_variable, 
                   input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, 
                   input$SB_Biotic_variable)
    datatable_function(all_varsY = all_varsY)  # Return raw data
  })
  table_subset_BFT <- reactive({
    all_varsY <- c("Year", input$BFT_recruitment_variable, input$BFT_distribution_variable, 
                   input$BFT_growth_variable, input$BFT_other_variable, input$BFT_Abiotic_variable, 
                   input$BFT_Biotic_variable)
    datatable_function(all_varsY = all_varsY)  # Return raw data
  })
  table_subset_AL <- reactive({
    all_varsY <- c("Year", input$AL_recruitment_variable, input$AL_distribution_variable, 
                   input$AL_growth_variable, input$AL_other_variable, input$AL_Abiotic_variable, 
                   input$AL_Biotic_variable)
    datatable_function(all_varsY = all_varsY)  # Return raw data
  })
  table_subset_AP <- reactive({
    all_varsY <- c("Year", input$AP_recruitment_variable, input$AP_distribution_variable, 
                   input$AP_growth_variable, input$AP_other_variable, input$AP_Abiotic_variable, 
                   input$AP_Biotic_variable)
    datatable_function(all_varsY = all_varsY)  # Return raw data
  })
  table_subset_AC <- reactive({
    all_varsY <- c("Year", input$AC_recruitment_variable, input$AC_distribution_variable, 
                   input$AC_growth_variable, input$AC_other_variable, input$AC_Abiotic_variable, 
                   input$AC_Biotic_variable)
    datatable_function(all_varsY = all_varsY)  # Return raw data
  })
  
  # Render DataTables
  output$mytable_SB <- DT::renderDataTable({
    table_subset_SB()
  })
  output$mytable_BFT <- DT::renderDataTable({
    table_subset_BFT()
  })
  output$mytable_AL <- DT::renderDataTable({
    table_subset_AL()
  })
  output$mytable_AP <- DT::renderDataTable({
    table_subset_AP()
  })
  output$mytable_AC <- DT::renderDataTable({
    table_subset_AC()
  })
  
  # Download handlers for CSV files
  output$downloadCSV_SB <- downloadHandler(
    filename = function() {
      paste("Stripedbass_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- table_subset_SB()  # Get raw data
      write.csv(data, file, row.names = FALSE)  # Save as CSV
    }
  )
  output$downloadCSV_BFT <- downloadHandler(
    filename = function() {
      paste("Bluefin_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- table_subset_BFT()  # Get raw data
      write.csv(data, file, row.names = FALSE)  # Save as CSV
    }
  )
  output$downloadCSV_AL <- downloadHandler(
    filename = function() {
      paste("Lobster_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- table_subset_AL()  # Get raw data
      write.csv(data, file, row.names = FALSE)  # Save as CSV
    }
  )
  output$downloadCSV_AP <- downloadHandler(
    filename = function() {
      paste("Plaice_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- table_subset_AP()  # Get raw data
      write.csv(data, file, row.names = FALSE)  # Save as CSV
    }
  )
  output$downloadCSV_AC <- downloadHandler(
    filename = function() {
      paste("Cod_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- table_subset_AC()  # Get raw data
      write.csv(data, file, row.names = FALSE)  # Save as CSV
    }
  )
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
