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
          h2(
            img(
              src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/2022-08/640x427-StripedBass-NOAAFisheries.png?itok=4ZQoQM0S",
              style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"
            ),
            HTML('<strong style="font-size: 40px;">Atlantic Striped Bass</strong> (<em>Morone saxatilis</em>)')
          ),
          mainPanel(
            width = 12,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Range & Info",
                fluidRow(
                  column(
                    width = 4,
                    HTML('<div style="width: 100%; height: 400px; background-color: black; margin-top: 30px;"></div>')
                  ),
                  column(
                    width = 8,
                    h3("Overview"),
                    p("Striped bass is an anadromous species native to the northeastern and central Atlantic coast of the United States and has a complex 
                      life cycle involving spawning in coastal rivers and estuaries, followed by long-range migrations to feeding and overwintering 
                      grounds. In Maine, striped bass fishing is a popular recreational activity, with the recreational harvest of striped bass routinely 
                      surpassing that of the commercial harvest. Historically, striped bass spawned in numerous rivers along coastal New England, but now 
                      rely on Chesapeake Bay and the Hudson River for the majority of spawning (Little 1995). Approximately 90% of the striped bass 
                      population spawn in the Chesapeake Bay region (Little 1995). The striped bass fishery supports numerous shore-side businesses for 
                      Maine’s economy, such as boat sales and rentals, bait and tackle shops, and fishing guide businesses. Striped bass landings are 
                      dependent upon the migration timing and persistence in Maine waters, which have experienced changes associated with ocean warming 
                      (Peer and Miller 2014; Secor et al. 2020). Changes in the timing and persistence of ecologically and economically important 
                      migratory species such as striped bass can lead to implications to both the management of this species, as well as the economic 
                      value of the fishery to the state.",
                      style = "font-size: 18px;text-align: justify;"),
                    h4("For more information, click the link below:", style = "padding-top: 40px;"),
                    tags$li(
                      HTML('<a href="C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/papers_writeups/StripedBassliteraturereview.pdf" style="font-size: 18px;" target="_blank">Environmental Effects on Striped Bass Stock Dynamics</a>')
                    )
                  )
                )
              ),
              tabPanel(
                "Interactive Plots",
                h2("Choose Stock and Environmental Variables", style = "font-weight: bold;"),
                fluidRow(
                  column(
                    width = 6,
                    h3("Stock Variables", style = "font-weight: bold;"),
                    h4("Variables related to recruitment", style = "font-weight: bold;"),
                    div(class = "fish-controls",fish_controls(c("Striped_Bass_Age1_Abundance", "Striped_Bass_Female_SSB"))),
                    h4("Variables related to growth", style = "font-weight: bold;"),
                    div(class = "fish-controls",fish_controls(c("Striped_Bass_WAA1_kg", "Striped_Bass_WAA4_kg"))),
                    h4("Other variables", style = "font-weight: bold;"),
                    div(class = "fish-controls",fish_controls(c("Striped_Bass_Total_Abundance", "Striped_Bass_Commercial_Landings",
                                                                 "Striped_Bass_Recreational_Landings", "Striped_Bass_Maine_Recreational_Harvest", "Striped_Bass_Full_F"))),
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
                    "NAO_ALL","Surface_Salinity_Mean"))),
                    h4("Biotic", style = "font-weight: bold;"),
                    div(class = "fish-controls",fish_controls(c("Annual_Forage_Fish_Biomass_GOM",
                                                                "Atlantic_herring_Fall_Depth_COG","Atlantic_herring_Fall_Lat_COG","Atlantic_herring_Spring_Depth_COG",   
                                                                "Atlantic_herring_Spring_Lat_COG","Calfin_anomaly_GOM","Large_Copepods_Abundance_Anomaly_GOM",
                                                                "Small_Copepods_Abundance_Anomaly_GOM")))
                  )),
                mainPanel(
                  width = 12,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Plot", plotlyOutput("plot"))
                  )
                )
              ),
              tabPanel(
                "Data",
                h2("View Data Tables"),
                sidebarPanel(
                  controls2,
                  h3("Select Species and Stock Variables"),
                  width = 12,
                  selectInput("Species", "Species", choices = c("Striped Bass" = "Striped_Bass")),
                  uiOutput("Stockdata_selector2")
                ),
                hr(style = "border-top: 1px solid #000000;"),
                mainPanel(width = 12, tabsetPanel(id = 'dataset', tabPanel("Data", DT::dataTableOutput("mytable1"))))
              )
            )
          )
        ), #close tabitem
        tabItem(
          tabName = "BluefinTuna",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/2022-09/640x427-Tuna-Bluefin-NOAAFisheries.png?itok=G5iVOYPt", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">Atlantic Bluefin Tuna</strong> (<em>Thunnus thynnus</em>)')
          ),
          sidebarPanel(
            controls,
            h3("Select Species and Stock Variables"),
            width = 12,
            selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass" = "Striped_Bass")),
            uiOutput("Stockdata_selector")
          ),
          radioButtons("Plotting_Style", "Select Plotting Style",
                       choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                       selected = "Stacked"),
          mainPanel(
            width = 12,
            tabsetPanel(
              type = "tabs",
              tabPanel("Plot", plotlyOutput("plot"))
            )
          )
        ), #close tabitem
        tabItem(
          tabName = "AmericanLobster",
          h2(
            img(src = "https://www.fisheries.noaa.gov/s3/styles/original/s3/dam-migration/640x427-american-lobster.png?itok=FX0oMipE", style = "width: 250px; height: auto; vertical-align: middle; margin-right: 10px;"),
            HTML('<strong style="font-size: 40px;">American Lobster</strong> (<em>Homarus americanus</em>)')
          ),
          sidebarPanel(
            controls,
            h3("Select Species and Stock Variables"),
            width = 12,
            selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass" = "Striped_Bass")),
            uiOutput("Stockdata_selector")
          ),
          radioButtons("Plotting_Style", "Select Plotting Style",
                       choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                       selected = "Stacked"),
          mainPanel(
            width = 12,
            tabsetPanel(
              type = "tabs",
              tabPanel("Plot", plotlyOutput("plot"))
            )
          )
        ), #close tabitem
        ####Source metadata code ####
        source(here("Code/app_metadata.R"), local = TRUE)$value
      )#close tabItems
    )#close fluidpage
  )#close dashboard body
)#close dashboardpage
###### Define server function  ######
server <- function(input, output,session) {
  dataDf <- reactive({
    temp <- Both
  })
  output$plot <- renderPlotly({
    num_variables <- length(c(input$variable, input$Stockdata))
    if (input$Plotting_Style == "Layered"){
      
      if (length(c(input$variable, input$Stockdata)) > 4){ #plot 5 variables together
        plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[3]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[5]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[5])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (length(c(input$variable, input$Stockdata)) == 4){ #if only 4 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[3]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[3])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (length(c(input$variable, input$Stockdata)) == 3){ #if only 3 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[3]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[3])) %>%
          layout(xaxis = list(title = "Year"))
      } else if (length(c(input$variable, input$Stockdata)) > 1){ #if only 2 variables are chosen
        plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1])) %>%
          add_trace(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                    type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2])) %>%
          layout(xaxis = list(title = "Year"))
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
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
        plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[i]), 
                type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[i]))
      })
      fig <- subplot(plot_list, nrows = num_variables, shareX = TRUE)
      
      # Set y-axis label based on selected checkbox labels
      y_labels <- c(input$variable, input$Stockdata)
      for (i in 1:num_variables) {
        fig <- fig %>% layout(yaxis = list(title = y_labels[i]))
      }
      
      layout(fig, xaxis = list(title = "Year"),
             plot_bgcolor = '#e5ecf6', 
             xaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))}
  })#close renderPlotly
  
  # choose columns to display
  #  output$mytable1 <- DT::renderDataTable({
  #    table2<-as.data.frame(Both2[, c(input$show_vars,input$Stockdata2), drop = FALSE])
  #DT::datatable(tail(table2,n=(second_col(table2)+5)))
  #    DT::datatable(tail(table2,n=(second_col(table2))))
  #  })
  output$mytable1 <- DT::renderDataTable({
    table2 <- as.data.frame(Both2[, c(input$show_vars, input$Stockdata2), drop = FALSE])
    
    if (length(input$show_vars) > 1) {
      # Find the index of the first non-NA value in the second longest column (when NA values are removed)
      sorted_vars <- input$show_vars[-1]  # Exclude "Year" from sorting
      sorted_vars_length <- sapply(sorted_vars, function(var) {
        non_na_indices <- which(!is.na(table2[[var]]))
        if (length(non_na_indices) > 0) {
          min(non_na_indices)
        } else {
          0  # Set length to 0 if all NA values
        }
      })
      sorted_vars <- sorted_vars[order(sorted_vars_length, decreasing = TRUE)]
      start_year <- min(which(!is.na(table2[[sorted_vars[2]]])))  # Find the index of the first non-NA value in the longest timeseries
      end_year <- nrow(table2)  # Total number of rows in the table2
      
      # Check if start_year is greater than the number of rows in table2
      if (start_year > nrow(table2)) {
        start_year <- min(which(!is.na(table2[[input$show_vars[1]]])))  # Display the entire column
      }
      
      # Subset the data from start_year to end_year
      table_subset <- table2[start_year:end_year, ]
    } else {
      # Only one variable selected, display the entire column
      start_year <- min(which(!is.na(table2[[input$show_vars[1]]])))
      table_subset <- table2[start_year:nrow(table2), ]
    }
    
    DT::datatable(table_subset)
  })
  
  ################Dropdown for Plots page##### 
  output$Stockdata_selector = renderUI({ #creates Species select box object called in ui
    selectInput(inputId = "Stockdata", #name of input
                label = "Stock Data:", #label displayed in ui
                choices = names(Both%>%select(grep(input$plotSpecies, names(.), ignore.case = T))),
                selected = NULL,multiple=TRUE)
  })
  ################Dropdown for Data page####
  output$Stockdata_selector2 = renderUI({ #creates Species select box object called in ui
    selectInput(inputId = "Stockdata2", #name of input
                label = "Stock Data:", #label displayed in ui
                choices = names(Both2[20:28]%>%select(grep(input$Species, names(.), ignore.case = T))),
                selected = NULL,multiple=TRUE)
  })
  ##############
  observe(print(input$accordion1))
  observe(print(input$accordion2))
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
