<<<<<<< HEAD
#### Load R packages ######
library(pacman)
pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here)
here()
#### SOURCE "automatic_ecodata.R"
source(here("Code/automatic_ecodata.R"))
big_ecodata<-lapply(ecodata_df,as.numeric)
####load ecodata that has been combined into one big df from "making_big_ecodata_df.R"#####
stock_assess_data<-read.csv(here("Data/stock_assess_data2.csv"))
stock_assess_data<-replace(stock_assess_data,stock_assess_data=='',NA)
stock_assess_data<-sapply(stock_assess_data,as.character)
stock_assess_data<- as.data.frame(gsub(",","",stock_assess_data))
stock_assess_data<-lapply(stock_assess_data,as.numeric)
#plaice_data<-read_excel(here("Data/Plaice_data_dependentvar.xlsx"))
##Merge all dfs
Both <- list(big_ecodata, stock_assess_data)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)

uniquecolnames <- unique(gsub("_[^_]+$", "", names(Both)[-1])) #get unique column names excluding year
#uniquecolnamesYEAR <- unique(gsub("_[^_]+$", "", names(Both))) #get unique column names including year

Both2<-as.data.frame(big_ecodata)
Both2<-Both2[ , order(names(Both2))]
Both2 <- list(Both[1],Both2, stock_assess_data)
Both2 <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both2)

###### tweaks, a list object to set up multicols for checkboxGroupInput#####
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 300px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))
##### data control: the checkboxes will control the data values plotted ######
controls <-
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("variable", 
                                   label = NULL, 
                                   choiceNames  = sort(gsub("_", " ", uniquecolnames[1:16]),decreasing = FALSE), ##replace_with space and select columns on env data only
                                   
                                   choiceValues = sort(colnames(Both[2:17]),decreasing = FALSE),  ##select columns on env data only
                                   selected = c("Bottom_Temp_Anomaly_GOM",multiple = TRUE)) 
                
       )#close tags$div
  )#close list 
##### controls2: year column included######
controls2 <-
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("show_vars", 
                                   label = NULL, 
                                   choiceNames  = gsub("_", " ", colnames(Both2[1:17])), ##select columns on env data only
                                   
                                   choiceValues = colnames(Both2[1:17]), ##select columns on env data only 
                                   selected = c("Year","Bottom_Temp_Anomaly_GOM",multiple = TRUE))
       )#close tags$div
  )#close list 
###### finding name of second longest column (excluding NAs)
second_col<- function (df){
  m1 = sapply(df, function(x) sum(!is.na(x))) #find length of each column
  m2 = m1[-which.max(m1)] #find second longest column
  mN = names(which.max(m2)) #get name of second longest column
  mP<-match(mN,names(df)) #match name of second longest column to column name of df
  mL = length(na.omit(df[,mP])) #reomve NAs of identified column of df and get length
  return(mL)
}
#second_col(Both)
########create sidebar: #########
sidebar <- dashboardSidebar(width = 150,
                            tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
                            sidebarMenu(
                              menuItem("Plots", tabName = "Plots", icon = icon("chart-line")),
                              menuItem("Data", tabName = "Data",icon = icon("table")),
                              menuItem("Metadata", tabName = "Metadata",icon = icon("list"))
                            )
)
######## define colors#####
gmri_colors<-tags$head(tags$style(HTML('

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #00608A;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #00736D;
        }
                              /* body */
                                .content-wrapper, .right-side {
                                background-color: #E9E9E9;
                                }


                              ')))
##########################

###### Define UI ######
ui <- dashboardPage(
  dashboardHeader(title= span("Visualizing Environmental Indicators in the Gulf of Maine", style = "color: #E9E9E9; font-size:34px;font-weight: bold;font-family: Arial"),titleWidth = 550,
                  
                  tags$li(a(href = 'https://www.gmri.org/',
                            img(src = "https://github.com/gulfofmaine/gmRi/blob/master/inst/stylesheets/gmri_logo.png?raw=true",
                                title = "gmri.org", height = "80px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown",
                          tags$style(".main-header {max-height: 100px}"),
                          tags$style(".main-header .logo {height: 100px}"))
  ),
  sidebar,
  dashboardBody(gmri_colors,
                fluidPage(tweaks,
                          tabItems(
                            
                            tabItem(tabName = "Plots",
                                    h2("Choose Variables & Plot (up to 5 Total)"),
                                    sidebarPanel(controls,
                                                 
                                                 h3("Select Species and Stock Variables"),width=12,
                                                 selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass"="Striped_Bass")),
                                                 uiOutput("Stockdata_selector")),# from objects created in server
                                    
                                    # Input: Select plotting style ----
                                    radioButtons("Plotting_Style", "Select Plotting Style",
                                                 choices = c("Layered" = "Layered","Stacked" = "Stacked"),
                                                 selected = 'Layered'),
                                    mainPanel(width = 12,
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Plot", plotlyOutput('plot'))
                                              )# Tabsetpanel
                                    ) #main panel
                            ), #tabItem
                            
                            tabItem(tabName = "Data",
                                    h2("View Data Tables"),
                                    sidebarPanel(controls2,
                                                 
                                                 h3("Select Species and Stock Variables"),width=12,
                                                 selectInput("Species", "Species", choices = c("Striped Bass"="Striped_Bass")),
                                                 uiOutput("Stockdata_selector2")),
                                    
                                    hr(style = "border-top: 1px solid #000000;"),
                                    mainPanel(width = 12,
                                              tabsetPanel(id = 'dataset',
                                                          tabPanel("Data", DT::dataTableOutput("mytable1"))))
                            ),
                            
                            #####Metadata#####
                            tabItem(tabName = "Metadata",
                                    h2("Metadata"),
                                    
                                    box( 
                                      accordion(
                                        id = "accordion1",
                                        accordionItem(
                                          title = "Annual AMO:",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "AMO (Atlantic Multidecadal Oscillation) Index. These data represent annual means of the NOAA Physical Sciences Laboratory's unsmoothed short monthly AMO dataset found at https://psl.noaa.gov/data/timeseries/AMO/. 1948-present.These data represent the weighted average over the N Atlantic from 0 to 70N, and have been detrended."
                                        ),
                                        accordionItem(
                                          title = "In-situ Bottom Temperature Anomaly (NEFSC)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual bottom temperature anomalies for the GOM region. Bottom Temperature data collected from NEFSC survey from 1977-present. 
                                          In ℃.Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Bottom Temperature Anomaly (GLORYS)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "GLORYS12V1 daily bottom temperature product anomalies for the GOM region. Annual means from 1993-2018. 
                                          1994-2010 climatology was used as anomaly period. In ℃.Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Calanus Abundance Anomaly",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Calanus finmarchicus abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info. "
                                        ),
                                        accordionItem(
                                          title = "Large Copepod Abundance Anomalies",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Large copepod abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Small Copepod Abundance Anomalies",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Small copepod abundance anomalies for the GOM region. Abundance anomalies estimated by averaging the individual abundance anomalies of Pseudocalanus spp., 
                                          Centropages hamatus, Centropages typicus, and Temora longicornis. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "GSI",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual time series of the Gulf Stream Index. Positive values are a more northerly Gulf Stream, and Negative values are a more southerly Gulf Stream. Anomalies of latitudinal position. 1954-present.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Hudson River Flow Rate (cubic meters per second)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Mean annual flow of the Hudson River in cubic meters per second at the USGS gauge 01358000 at Green Island, New York.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "NAO",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "North Atlantic Oscillation (NAO). Unit-less. 1864-present. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "OISST Anomaly (Season)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "SST anomalies for the GOM region for either the winter, spring, summer, or fall season. These data were derived from the NOAA Optimum Interpolation SST High Resolution data set (NOAA OISST V2). 
                                          The 1982-2020 climatology was used to calculate anomalies.1982-present. In ℃.  Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "In-situ SST Anomaly (NEFSC)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual SST anomalies for the GOM region. SST data collected from NEFSC survey from 1977-present. 
                                          In ℃. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        )

                                      ),#accordion
                                      title = "Environmental Data", footer = NULL, status = "success",
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
                                      #############
                                      title = "Stock Data", footer = NULL, status = "success",
                                      solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                                      collapsible = TRUE, collapsed = TRUE)#box
                            )#tabitem
                          )#tabitems
                          
                          ############  
                )#fluidpage
  )#dasboardbody
) # dashboard page


###### Define server function  ######
server <- function(input, output,session) {
  dataDf <- reactive({
    temp <- Both
  })
  output$plot <- renderPlotly({
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
      }#close if layered option
    } else { 
      if (length(c(input$variable, input$Stockdata)) > 4){ #plot 5 variables together
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter',mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter',mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        fig4<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4]))
        fig5<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[5]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[5]))
        
        
        fig<-subplot(fig1, fig2,fig3,fig4,fig5, nrows = 5, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"), 
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if (length(c(input$variable, input$Stockdata)) == 4){ #if only 4 variables are chosen
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        fig4<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4]))
        
        
        fig<-subplot(fig1, fig2,fig3,fig4, nrows = 4, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if (length(c(input$variable, input$Stockdata)) == 3){ #if only 3 variables are chosen
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        
        
        fig<-subplot(fig1, fig2,fig3,nrows = 3, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if(length(c(input$variable, input$Stockdata)) == 2){ #plot 2 variables together
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        
        
        fig<-subplot(fig1, fig2, nrows = 2, shareX =TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig<-subplot(fig1, nrows = 1) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      }
    }#close if stacked option
    
  }) #close renderPlotly
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    table2<-as.data.frame(Both2[, c(input$show_vars,input$Stockdata2), drop = FALSE])
    #DT::datatable(tail(table2,n=second_col(table2)))
    DT::datatable(tail(table2,n=(second_col(table2)+5)))
  })
  
  ################Dropdown for Plots page##### 
 # Striped_Bass<-gsub("_", " ", names(Both%>%select(grep("Striped_Bass", names(.), ignore.case = T))))
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
                #choices = as.character(unique(stockData$Species)),
                choices = names(Both2[18:24]%>%select(grep(input$Species, names(.), ignore.case = T))),
                selected = NULL,multiple=TRUE)
  })
  ##############
  observe(print(input$accordion1))
  observe(print(input$accordion2))
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
=======
#### Load R packages ######
library(pacman)
pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here)
here()
#### SOURCE "automatic_ecodata.R"
source(here("Code/automatic_ecodata.R"))
big_ecodata<-lapply(ecodata_df,as.numeric)
####load ecodata that has been combined into one big df from "making_big_ecodata_df.R"#####
stock_assess_data<-read.csv(here("Data/stock_assess_data2.csv"))
stock_assess_data<-replace(stock_assess_data,stock_assess_data=='',NA)
stock_assess_data<-sapply(stock_assess_data,as.character)
stock_assess_data<- as.data.frame(gsub(",","",stock_assess_data))
stock_assess_data<-lapply(stock_assess_data,as.numeric)
#plaice_data<-read_excel(here("Data/Plaice_data_dependentvar.xlsx"))
##Merge all dfs
Both <- list(big_ecodata, stock_assess_data)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)

uniquecolnames <- unique(gsub("_[^_]+$", "", names(Both)[-1])) #get unique column names excluding year
#uniquecolnamesYEAR <- unique(gsub("_[^_]+$", "", names(Both))) #get unique column names including year

Both2<-as.data.frame(big_ecodata)
Both2<-Both2[ , order(names(Both2))]
Both2 <- list(Both[1],Both2, stock_assess_data)
Both2 <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both2)

###### tweaks, a list object to set up multicols for checkboxGroupInput#####
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 300px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))
##### data control: the checkboxes will control the data values plotted ######
controls <-
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("variable", 
                                   label = NULL, 
                                   choiceNames  = sort(gsub("_", " ", uniquecolnames[1:16]),decreasing = FALSE), ##replace_with space and select columns on env data only
                                   
                                   choiceValues = sort(colnames(Both[2:17]),decreasing = FALSE),  ##select columns on env data only
                                   selected = c("Bottom_Temp_Anomaly_GOM",multiple = TRUE)) 
                
       )#close tags$div
  )#close list 
##### controls2: year column included######
controls2 <-
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("show_vars", 
                                   label = NULL, 
                                   choiceNames  = gsub("_", " ", colnames(Both2[1:17])), ##select columns on env data only
                                   
                                   choiceValues = colnames(Both2[1:17]), ##select columns on env data only 
                                   selected = c("Year","Bottom_Temp_Anomaly_GOM",multiple = TRUE))
       )#close tags$div
  )#close list 
###### finding name of second longest column (excluding NAs)
second_col<- function (df){
  m1 = sapply(df, function(x) sum(!is.na(x))) #find length of each column
  m2 = m1[-which.max(m1)] #find second longest column
  mN = names(which.max(m2)) #get name of second longest column
  mP<-match(mN,names(df)) #match name of second longest column to column name of df
  mL = length(na.omit(df[,mP])) #reomve NAs of identified column of df and get length
  return(mL)
}
#second_col(Both)
########create sidebar: #########
sidebar <- dashboardSidebar(width = 150,
                            tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
                            sidebarMenu(
                              menuItem("Plots", tabName = "Plots", icon = icon("chart-line")),
                              menuItem("Data", tabName = "Data",icon = icon("table")),
                              menuItem("Metadata", tabName = "Metadata",icon = icon("list"))
                            )
)
######## define colors#####
gmri_colors<-tags$head(tags$style(HTML('

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #00608A;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #00736D;
        }
                              /* body */
                                .content-wrapper, .right-side {
                                background-color: #E9E9E9;
                                }


                              ')))
##########################

###### Define UI ######
ui <- dashboardPage(
  dashboardHeader(title= span("Visualizing Environmental Indicators in the Gulf of Maine", style = "color: #E9E9E9; font-size:34px;font-weight: bold;font-family: Arial"),titleWidth = 550,
                  
                  tags$li(a(href = 'https://www.gmri.org/',
                            img(src = "https://github.com/gulfofmaine/gmRi/blob/master/inst/stylesheets/gmri_logo.png?raw=true",
                                title = "gmri.org", height = "80px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown",
                          tags$style(".main-header {max-height: 100px}"),
                          tags$style(".main-header .logo {height: 100px}"))
  ),
  sidebar,
  dashboardBody(gmri_colors,
                fluidPage(tweaks,
                          tabItems(
                            
                            tabItem(tabName = "Plots",
                                    h2("Choose Variables & Plot (up to 5 Total)"),
                                    sidebarPanel(controls,
                                                 
                                                 h3("Select Species and Stock Variables"),width=12,
                                                 selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass"="Striped_Bass")),
                                                 uiOutput("Stockdata_selector")),# from objects created in server
                                    
                                    # Input: Select plotting style ----
                                    radioButtons("Plotting_Style", "Select Plotting Style",
                                                 choices = c("Layered" = "Layered","Stacked" = "Stacked"),
                                                 selected = 'Layered'),
                                    mainPanel(width = 12,
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Plot", plotlyOutput('plot'))
                                              )# Tabsetpanel
                                    ) #main panel
                            ), #tabItem
                            
                            tabItem(tabName = "Data",
                                    h2("View Data Tables"),
                                    sidebarPanel(controls2,
                                                 
                                                 h3("Select Species and Stock Variables"),width=12,
                                                 selectInput("Species", "Species", choices = c("Striped Bass"="Striped_Bass")),
                                                 uiOutput("Stockdata_selector2")),
                                    
                                    hr(style = "border-top: 1px solid #000000;"),
                                    mainPanel(width = 12,
                                              tabsetPanel(id = 'dataset',
                                                          tabPanel("Data", DT::dataTableOutput("mytable1"))))
                            ),
                            
                            #####Metadata#####
                            tabItem(tabName = "Metadata",
                                    h2("Metadata"),
                                    
                                    box( 
                                      accordion(
                                        id = "accordion1",
                                        accordionItem(
                                          title = "Annual AMO:",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "AMO (Atlantic Multidecadal Oscillation) Index. These data represent annual means of the NOAA Physical Sciences Laboratory's unsmoothed short monthly AMO dataset found at https://psl.noaa.gov/data/timeseries/AMO/. 1948-present.These data represent the weighted average over the N Atlantic from 0 to 70N, and have been detrended."
                                        ),
                                        accordionItem(
                                          title = "In-situ Bottom Temperature Anomaly (NEFSC)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual bottom temperature anomalies for the GOM region. Bottom Temperature data collected from NEFSC survey from 1977-present. 
                                          In ℃.Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Bottom Temperature Anomaly (GLORYS)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "GLORYS12V1 daily bottom temperature product anomalies for the GOM region. Annual means from 1993-2018. 
                                          1994-2010 climatology was used as anomaly period. In ℃.Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Calanus Abundance Anomaly",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Calanus finmarchicus abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info. "
                                        ),
                                        accordionItem(
                                          title = "Large Copepod Abundance Anomalies",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Large copepod abundance anomalies for the GOM region. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Small Copepod Abundance Anomalies",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Small copepod abundance anomalies for the GOM region. Abundance anomalies estimated by averaging the individual abundance anomalies of Pseudocalanus spp., 
                                          Centropages hamatus, Centropages typicus, and Temora longicornis. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "GSI",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual time series of the Gulf Stream Index. Positive values are a more northerly Gulf Stream, and Negative values are a more southerly Gulf Stream. Anomalies of latitudinal position. 1954-present.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "Hudson River Flow Rate (cubic meters per second)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Mean annual flow of the Hudson River in cubic meters per second at the USGS gauge 01358000 at Green Island, New York.
                                          Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "NAO",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "North Atlantic Oscillation (NAO). Unit-less. 1864-present. Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "OISST Anomaly (Season)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "SST anomalies for the GOM region for either the winter, spring, summer, or fall season. These data were derived from the NOAA Optimum Interpolation SST High Resolution data set (NOAA OISST V2). 
                                          The 1982-2020 climatology was used to calculate anomalies.1982-present. In ℃.  Data are sourced from the R package 'ecodata'. 
                                          See https://noaa-edab.github.io/tech-doc/ for more info."
                                        ),
                                        accordionItem(
                                          title = "In-situ SST Anomaly (NEFSC)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual SST anomalies for the GOM region. SST data collected from NEFSC survey from 1977-present. 
                                          In ℃. Data are sourced from the R package 'ecodata'. See https://noaa-edab.github.io/tech-doc/ for more info."
                                        )

                                      ),#accordion
                                      title = "Environmental Data", footer = NULL, status = "success",
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
                                      #############
                                      title = "Stock Data", footer = NULL, status = "success",
                                      solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                                      collapsible = TRUE, collapsed = TRUE)#box
                            )#tabitem
                          )#tabitems
                          
                          ############  
                )#fluidpage
  )#dasboardbody
) # dashboard page


###### Define server function  ######
server <- function(input, output,session) {
  dataDf <- reactive({
    temp <- Both
  })
  output$plot <- renderPlotly({
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
      }#close if layered option
    } else { 
      if (length(c(input$variable, input$Stockdata)) > 4){ #plot 5 variables together
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter',mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter',mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        fig4<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4]))
        fig5<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[5]), 
                       type = 'scatter',mode = 'lines',name = paste(c(input$variable, input$Stockdata)[5]))
        
        
        fig<-subplot(fig1, fig2,fig3,fig4,fig5, nrows = 5, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"), 
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if (length(c(input$variable, input$Stockdata)) == 4){ #if only 4 variables are chosen
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        fig4<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[4]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[4]))
        
        
        fig<-subplot(fig1, fig2,fig3,fig4, nrows = 4, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if (length(c(input$variable, input$Stockdata)) == 3){ #if only 3 variables are chosen
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        fig3<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[3]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[3]))
        
        
        fig<-subplot(fig1, fig2,fig3,nrows = 3, shareX = TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else if(length(c(input$variable, input$Stockdata)) == 2){ #plot 2 variables together
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        
        
        fig<-subplot(fig1, fig2, nrows = 2, shareX =TRUE) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      } else { #plot individually if only 1 is selected
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig<-subplot(fig1, nrows = 1) %>% 
          layout(xaxis = list(title = "Year"),
                 plot_bgcolor='#e5ecf6', 
                 xaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'), 
                 yaxis = list( 
                   zerolinecolor = '#ffff', 
                   zerolinewidth = 2, 
                   gridcolor = 'ffff'))
        fig
      }
    }#close if stacked option
    
  }) #close renderPlotly
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    table2<-as.data.frame(Both2[, c(input$show_vars,input$Stockdata2), drop = FALSE])
    #DT::datatable(tail(table2,n=second_col(table2)))
    DT::datatable(tail(table2,n=(second_col(table2)+5)))
  })
  
  ################Dropdown for Plots page##### 
 # Striped_Bass<-gsub("_", " ", names(Both%>%select(grep("Striped_Bass", names(.), ignore.case = T))))
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
                #choices = as.character(unique(stockData$Species)),
                choices = names(Both2[18:24]%>%select(grep(input$Species, names(.), ignore.case = T))),
                selected = NULL,multiple=TRUE)
  })
  ##############
  observe(print(input$accordion1))
  observe(print(input$accordion2))
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
>>>>>>> 0ea69a5afffce8fd03ce138fb9bac624df60808f
