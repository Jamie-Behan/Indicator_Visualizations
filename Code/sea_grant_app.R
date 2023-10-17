#### Load R packages ######
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here,install=TRUE)
here()
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
                                        "Plotting_Style",
                                        "Select Plotting Style",
                                        choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                                        selected = "Stacked"
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
                                    plotlyOutput("stripedbass_plot")
                                  ))))), #close tabitem
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
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_Age1_Abundance", "Striped_Bass_Female_SSB"),var_name="BFT_recruitment_variable")),
                                        h4("Variables related to growth", style = "font-weight: bold;"),
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_WAA1_kg", "Striped_Bass_WAA4_kg"),var_name="BFT_growth_variable")),
                                        h4("Other variables", style = "font-weight: bold;"),
                                        #div(class = "fish-controls",fish_controls(c("Striped_Bass_Total_Abundance", "Striped_Bass_Commercial_Landings",
                                        #                                            "Striped_Bass_Recreational_Landings", "Striped_Bass_Maine_Recreational_Harvest", "Striped_Bass_Full_F"),var_name="BFT_other_variable")),
                                        radioButtons(
                                          "Plotting_Style",
                                          "Select Plotting Style",
                                          choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                                          selected = "Stacked"
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
                                      plotlyOutput("BFT_plot"))
          ))#Tabpanel
                      )#main panel
        ), #close tabitem
        tabItem(
          tabName = "AmericanLobster",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/dam-migration/640x427-american-lobster.png?itok=FX0oMipE", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">American Lobster</strong> (<em>Homarus americanus</em>)')
          )), #close tabitem
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
    num_variables <- length(c(input$SB_recruitment_variable,input$SB_growth_variable,input$SB_other_variable, input$SB_Abiotic_variable,input$SB_Biotic_variable))
    all_vars<-c(input$SB_recruitment_variable,input$SB_growth_variable,input$SB_other_variable, input$SB_Abiotic_variable,input$SB_Biotic_variable)
    if (input$Plotting_Style == "Layered") {
      
      if (num_variables > 4){ #plot 5 variables together
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[5]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[5])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 4){ #if only 4 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 3){ #if only 3 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables > 1){ #if only 2 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          layout(xaxis = list(title = "Year"))
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                       type = 'scatter', mode = 'lines', name = paste(all_vars[1]))
        fig<-subplot(fig1, nrows = 1) %>% 
          layout(xaxis = list(title = "Year"),
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      }}#close if layered option
    else {
      plot_list <- lapply(1:num_variables, function(i) {
        plot_ly(dataDf(), x = ~Year, y = ~get(all_vars[i]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[i]))
      })
      fig <- subplot(plot_list, nrows = num_variables, shareX = TRUE)
      
      # Set y-axis label based on selected checkbox labels
      y_labels <- all_vars
      for (i in 1:num_variables) {
        fig <- fig %>% layout(yaxis = list(title = y_labels[i]))
      }
      
      layout(fig, xaxis = list(title = "Year"),
             plot_bgcolor = '#e5ecf6', 
             xaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))}
  })#close renderPlotly
  output$BFT_plot <- renderPlotly({
    num_variables <- length(c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable))
    all_vars<-c(input$BFT_recruitment_variable,input$BFT_growth_variable,input$BFT_other_variable, input$BFT_Abiotic_variable,input$BFT_Biotic_variable)
    if (input$Plotting_Style == "Layered") {
      
      if (num_variables > 4){ #plot 5 variables together
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[5]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[5])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 4){ #if only 4 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 3){ #if only 3 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables > 1){ #if only 2 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          layout(xaxis = list(title = "Year"))
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                       type = 'scatter', mode = 'lines', name = paste(all_vars[1]))
        fig<-subplot(fig1, nrows = 1) %>% 
          layout(xaxis = list(title = "Year"),
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      }}#close if layered option
    else {
      plot_list <- lapply(1:num_variables, function(i) {
        plot_ly(dataDf(), x = ~Year, y = ~get(all_vars[i]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[i]))
      })
      fig <- subplot(plot_list, nrows = num_variables, shareX = TRUE)
      
      # Set y-axis label based on selected checkbox labels
      y_labels <- all_vars
      for (i in 1:num_variables) {
        fig <- fig %>% layout(yaxis = list(title = y_labels[i]))
      }
      
      layout(fig, xaxis = list(title = "Year"),
             plot_bgcolor = '#e5ecf6', 
             xaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))}
  })#close renderPlotly
  output$lobster_plot <- renderPlotly({
    num_variables <- length(c(input$recruitment_variable,input$growth_variable,input$other_variable, input$Abiotic_variable,input$Biotic_variable))
    all_vars<-c(input$recruitment_variable,input$growth_variable,input$other_variable, input$Abiotic_variable,input$Biotic_variable)
    if (input$Plotting_Style == "Layered") {
      
      if (num_variables > 4){ #plot 5 variables together
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[5]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[5])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 4){ #if only 4 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[4])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables == 3){ #if only 3 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[3])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (num_variables > 1){ #if only 2 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                    type = 'scatter', mode = 'lines',name = paste(all_vars[2])) %>%
          layout(xaxis = list(title = "Year"))
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
                       type = 'scatter', mode = 'lines', name = paste(all_vars[1]))
        fig<-subplot(fig1, nrows = 1) %>% 
          layout(xaxis = list(title = "Year"),
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      }}#close if layered option
    else {
      plot_list <- lapply(1:num_variables, function(i) {
        plot_ly(dataDf(), x = ~Year, y = ~get(all_vars[i]), 
                type = 'scatter', mode = 'lines', name = paste(all_vars[i]))
      })
      fig <- subplot(plot_list, nrows = num_variables, shareX = TRUE)
      
      # Set y-axis label based on selected checkbox labels
      y_labels <- all_vars
      for (i in 1:num_variables) {
        fig <- fig %>% layout(yaxis = list(title = y_labels[i]))
      }
      
      layout(fig, xaxis = list(title = "Year"),
             plot_bgcolor = '#e5ecf6', 
             xaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))}
  })#close renderPlotly
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
