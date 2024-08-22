#### Load R packages ######
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("gulfofmaine/gmRi")
#remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
library(here)
library(DT)
library(shiny)
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
#pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here,install=TRUE,purrr)
#pacman::p_load(akima,sdm,writexl,sf)
##### LOAD DATA ######
source(here("Code/Compile_data.R"))

##### LOAD supporting functions ########
source(here("Code/supporting_functions_for_app.R"))

##########################
###### Define UI ######
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
                                      div(class = "fish-controls",fish_controls(c("Striped_Bass_WAA1_kg", "Striped_Bass_WAA4_kg"),var_name="SB_growth_variable")),
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
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      h3("Environmental Variables", style = "font-weight: bold;"),
                                      h4("Abiotic", style = "font-weight: bold;"),
                                      div(class = "fish-controls",fish_controls(c("AMO_Annual_Mean","Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM",
                                                                                  "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM","GSI_ALL","Hudson_River_Flow_Rate_MAB",
                                                                                  "NAO_ALL","Surface_Salinity_Mean"),var_name="SB_Abiotic_variable")),
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
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_WAA1_kg", "Striped_Bass_WAA4_kg"),var_name="BFT_growth_variable")),
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
                                        )
                                      ),
                                      column(
                                        width = 6,
                                        h3("Environmental Variables", style = "font-weight: bold;"),
                                        h4("Abiotic", style = "font-weight: bold;"),
                                        div(class = "fish-controls",fish_controls(c("AMO_Annual_Mean","Annual_Bottom_Temp_Absolute_GOM","SST_Temp_Anomaly_GOM",
                                                                                    "Bottom_Temp_Anomaly_GOM","GLORYS_Bottom_Temp_Anomaly_GOM","GSI_ALL",
                                                                                    "NAO_ALL","Surface_Salinity_Mean"),var_name="BFT_Abiotic_variable")),
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
                                        )
                                      ),
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
        ####Source metadata code ####
        source(here("Code/app_metadata.R"), local = TRUE)$value
      )#close tabItems
    )#close fluidpage
  )#close dashboard body
)#close dashboardpage
###### Define server function  ######
server <- function(input, output, session) {
  
  dataDf <- reactive({
    temp <- Both2
  })
  output$stripedbass_plot <- renderPlotly({
    num_variables <- length(c(input$SB_recruitment_variable, input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, input$SB_Biotic_variable))
    all_vars <- c(input$SB_recruitment_variable, input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, input$SB_Biotic_variable)
    plot_function(plottingstyle = input$SB_Plotting_Style, num_variables = num_variables, dataDf = dataDf, all_vars = all_vars, name_mapping = name_mapping, name_mapping2 = name_mapping2, show_trendline=input$trendline_SB)

    })
  output$BFT_plot <- renderPlotly({
    num_variables <- length(c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable))
    all_vars<-c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable)
    plot_function(plottingstyle = input$BFT_Plotting_Style,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline=input$trendline_BFT)
  })#close renderPlotly
  output$AL_plot <- renderPlotly({
    num_variables <- length(c(input$AL_recruitment_variable,input$AL_distribution_variable,input$AL_other_variable, input$AL_Abiotic_variable,input$AL_Biotic_variable))
    all_vars<-c(input$AL_recruitment_variable,input$AL_distribution_variable,input$AL_other_variable, input$AL_Abiotic_variable,input$AL_Biotic_variable)
    plot_function(plottingstyle = input$AL_Plotting_Style,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline=input$trendline_AL)
  })#close renderPlotly
  
  output$mytable_SB <- DT::renderDataTable({
    all_varsY <- c("Year", input$SB_recruitment_variable, input$SB_growth_variable, input$SB_other_variable, input$SB_Abiotic_variable, input$SB_Biotic_variable)
    datatable_function(all_varsY=all_varsY)
  })
  output$mytable_BFT <- DT::renderDataTable({
    all_varsY <- c("Year", input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable)
    datatable_function(all_varsY=all_varsY)
  })
  output$mytable_AL <- DT::renderDataTable({
    all_varsY <- c("Year", input$AL_recruitment_variable,input$AL_distribution_variable,input$AL_other_variable, input$AL_Abiotic_variable,input$AL_Biotic_variable)
    datatable_function(all_varsY=all_varsY)
  })
  output$downloadCSV_SB <- downloadHandler(
    filename = function() {
      paste("Stripedbass_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_subset, file)
    }
  )
  output$downloadCSV_BFT <- downloadHandler(
    filename = function() {
      paste("Bluefin_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_subset, file)
    }
  )
  output$downloadCSV_AL <- downloadHandler(
    filename = function() {
      paste("Lobster_indicator_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_subset, file)
    }
  )
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)