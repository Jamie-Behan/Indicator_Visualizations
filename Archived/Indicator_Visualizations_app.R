#### Load R packages ######
library(pacman)
pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here)
here()

####load ecodata that has been combined into one big df from "making_big_ecodata_df.R"#####
big_ecodata<-read.csv(here("Data/big_ecodata.csv"))
stock_assess_data<-read.csv(here("Data/stock_assess_data.csv"))
plaice_data<-read_excel(here("Data/Plaice_data_dependentvar.xlsx"))

big_ecodata<-big_ecodata[,c(2,6,3,16,21,19,20,18,13,17,4,5,14,15,22,23,10,11,12,7,8,9,1)] #reorder columns
##Merge all dfs
Both <- list(big_ecodata, stock_assess_data,plaice_data)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)
Both<- Both[-c(190),] ## get rid of 2022 year row

colnames(Both)<-c("Year","Bottom Temperature Anomaly","Bottom Temperature Anomaly (GLORYS)", "SST Anomaly", "Longterm SST","OISST Anomaly Winter", "OISST Anomaly Spring","OISST Anomaly Summer","OISST Anomaly Fall","GSI","NAO","Bottom Heatwave Cumulative Intensity","Bottom Heatwave Maximum Intensity","Heatwave Cumulative Intensity","Heatwave Maximum Intensity","Warm Slope Water", "Labrador Slope Water", "Calanus c5 Spring", "Calanus c5 Summer", "Calanus c5 Fall", "Calanus Adult Spring", "Calanus Adult Summer", "Calanus Adult Fall","Haddock Commercial Landings (mt)","Haddock NMFS Fall (numbers/tow)","Haddock NMFS Spring (numbers/tow)","Shortfin Squid Commercial Landings (mt)","Shortfin Squid DFO Spring (numbers/tow)","Shortfin Squid DFO Fall (numbers/tow)","Plaice Age 1 Spring (num/tow)","Plaice Age 1 Fall (num/tow)","Plaice Spring SSB NEFSC (kg/tow)","Plaice Fall SSB NEFSC (kg/tow)","Plaice Spring R/SSB","Plaice Fall R/SSB","Plaice Mean Condition","Plaice Standard Deviation of Mean Condition","Plaice Spring Mean Depth","Plaice Fall Mean Depth","Plaice Spring Mean Latitude","Plaice Fall Mean Latitude")

Both2<-Both
Both2<-Both2[ , order(names(Both2))]
colnames(Both2)[24:35]<-paste("American",colnames(Both2[,c(24:35)]), sep = " ")

###### tweaks, a list object to set up multicols for checkboxGroupInput#####
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 310px;
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
  list(h3("Select Variables (up to 5 Total)"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("variable", 
                                   label = NULL, 
                                   choiceNames  = sort(colnames(Both[2:23]),decreasing = FALSE),
                                   
                                   choiceValues = sort(colnames(Both[2:23]),decreasing = FALSE), 
                                   selected = c("Bottom Temperature Anomaly",multiple = TRUE)) 
                
       )#close tags$div
  )#close list 
##### controls2: year column included######
controls2 <-
  list(h3("Select Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("show_vars", 
                                   label = NULL, 
                                   choiceNames  = colnames(Both2[c(1:11,15:23,39:41)]),
                                   
                                   choiceValues = colnames(Both2[c(1:11,15:23,39:41)]), 
                                   selected = c("Year","Bottom Temperature Anomaly",multiple = TRUE))
       )#close tags$div
  )#close list 
#######list of variables that use certain Y axes####
anom_var<-list("Bottom Temperature Anomaly","Bottom Temperature Anomaly (GLORYS)","SST Anomaly","OISST Anomaly Winter","OISST Anomaly Spring","OISST Anomaly Summer","OISST Anomaly Fall")
calanus_var<-list("Calanus c5 Spring", "Calanus c5 Summer","Calanus c5 Fall", "Calanus Adult Spring","Calanus Adult Summer", "Calanus Adult Fall")
water_var<-list("Warm Slope Water", "Labrador Slope Water")
###### finding name of second longest column (excluding NAs)
second_col<- function (df){
  m1 = sapply(df, function(x) sum(!is.na(x))) #find length of each column
  m2 = m1[-which.max(m1)]
  mN = names(which.max(m2))
  #mP<-grep(mN, colnames(df))
  mP<-match(mN,names(df))
  mL = length(na.omit(df[,mP]))
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
  dashboardHeader(title= span("Visualizing Indicators in the GOM", style = "color: #E9E9E9; font-size:34px;font-weight: bold;font-family: Avenir"),titleWidth = 350,
                  
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
                                    h2("Choose Variables & Plot"),
                                    sidebarPanel(controls,
                                                 
                                                 h3("Select Species and Stock Variables"),width=12,
                                                 selectInput("plotSpecies", "plotSpecies", choices = c("Haddock", "Shortfin Squid", "Plaice")),
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
                                                 selectInput("Species", "Species", choices = c("Haddock", "Shortfin Squid", "American Plaice")),
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
                                          title = "Bottom Heatwave Cumulative Intensity:",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Bottom cumulative heatwave intensity each year.1959-2019. In ℃."
                                        ),
                                        accordionItem(
                                          title = "Bottom Heatwave Maximum Intensity:",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Bottom Maximums. There may be more than one maximum value per year. 1959-2019. In ℃."
                                        ),
                                        accordionItem(
                                          title = "Bottom Temperature Anomaly",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual bottom temperature anomalies for the Northeast U.S Shelf. 1977-2021. In ℃."
                                        ),
                                        accordionItem(
                                          title = "Bottom Temperature Anomaly (GLORYS)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "GLORYS Annual bottom temperature anomalies. 1993-2018. In ℃."
                                        ),
                                        accordionItem(
                                          title = "Calanus Adult (Season)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Calanus finmarchicus abundance at adult stage in (season: spring, summer, fall, or winter). 1977-2019."
                                        ),
                                        accordionItem(
                                          title = "Calanus c5 (Season)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Calanus finmarchicus abundance at c5 stage in (season: spring, summer, fall, or winter). 1977-2019."
                                        ),
                                        accordionItem(
                                          title = "GSI",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual time series of the Gulf Stream Index. Positive values are a more northerly Gulf Stream, and Negative values are a more southerly Gulf Stream. Anomalies of latitudinal position.1954-2020."
                                        ),
                                        accordionItem(
                                          title = "Heatwave Cumulative Intensity",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Cumulative intensity marine heatwaves on the NES. Data are inconsistently taken from 1982-2021."
                                        ),
                                        accordionItem(
                                          title = "Heatwave Maximum Intensity",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Maximum intensity marine heatwaves on the NES. There may be more than one maximum value per year. Data are inconsistently taken from 1982-2021."
                                        ),
                                        accordionItem(
                                          title = "Labrador Slope Water",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Percent of total Labrador Slope Water observed in the deep Northeast Channel (150-200 m water depth).1977-2021."
                                        ),
                                        accordionItem(
                                          title = "Longterm SST",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Long-term SSTs were derived from the NOAA extended reconstructed SST data set (ERSST V5). ERSST V5 data set is parsed into gridded bins between 1954-present with monthly temporal resolution. 1854-2021. In ℃."
                                        ),
                                        accordionItem(
                                          title = "NAO",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "North Atlantic Oscillation (NAO). Unit-less.1864-2018."
                                        ),
                                        accordionItem(
                                          title = "OISST Anomaly (Season)",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "SST anomalies on the Northeast Continental Shelf for either the winter, spring, summer, or fall season. These data were derived from the NOAA Optimum Interpolation SST High Resolution data set (NOAA OISST V2). The 1982-2020 climatology was used to calculate anomalies.1982-2021. In ℃."
                                        ),
                                        accordionItem(
                                          title = "SST Anomaly",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Annual SST anomalies for the Northeast U.S Shelf. 1977-2021. In ℃."
                                        ),
                                        accordionItem(
                                          title = "Warm Slope Water",
                                          status = "primary",
                                          collapsed = FALSE,
                                          "Percent of total Warm Slope Water observed in the deep Northeast Channel (150-200 m water depth) .1977-2021."
                                        )
                                      ),#accordion
                                      title = "Environmental Data", footer = NULL, status = "success",
                                      solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                                      collapsible = TRUE, collapsed = TRUE)#box
                                    ,
                                    
                                    box(  
                                      #####Haddoack accordion#####
                                      accordion(
                                        id = "accordion2",
                                        accordionItem(
                                          title = "Haddock (Melanogrammus aeglefinus):",
                                          status = "success",
                                          collapsed = TRUE,
                                          accordionItem(
                                            title = "Haddock Commercial Landings (mt):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Haddock commercial landings in metric tons. 1977-2019."
                                          ),
                                          accordionItem(
                                            title = "Haddock NMFS Fall (numbers/tow):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Haddock Fall NMFS survey data in numbers/tow. 1963-2019."
                                          ),
                                          accordionItem(
                                            title = "Haddock NMFS Spring (numbers/tow):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Haddock Spring NMFS survey data in numbers/tow. 1968-2019."
                                          )
                                        ),
                                        ###### Shortfin Squid Accordion#####
                                        accordionItem(
                                          title = "Shortfin Squid (Illex illecebrosus):",
                                          status = "success",
                                          collapsed = TRUE,
                                          accordionItem(
                                            title = "Shortfin Squid DFO Fall (numbers/tow):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Shortfin Squid Fall DFO survey. units are in numbers/tow."
                                          ),
                                          accordionItem(
                                            title = "Shortfin Squid DFO Spring (numbers/tow):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Shortfin Squid Spring DFO survey. units are in numbers/tow."
                                          ),
                                          accordionItem(
                                            title = "Shortfin Squid Landings (mt):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Shortfin Squid landings in metric tons."
                                          )
                                        ),
                                        #######American plaice accordion####
                                        accordionItem(
                                          title = "American Plaice (Hippoglossoides platessoides):",
                                          status = "success",
                                          collapsed = TRUE,
                                          accordionItem(
                                            title = "Plaice Mean Condition:",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American Plaice mean relative condition. These data represent the ratio of observed weight to predicted weight from the Fall NEFSC trawl survey and span from 1992-2019.
"
                                          ),
                                          accordionItem(
                                            title = "Plaice Mean Condition StdDev:",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "Standard deviation of the AM plaice mean relative condition data."
                                          ),
                                          accordionItem(
                                            title = "Plaice (Season) Age1 (numbers/tow):",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American Plaice Age 1 fall or spring NEFSC survey (recruitment). units are in numbers/tow."
                                          ),
                                          accordionItem(
                                            title = "Plaice (Season) Mean Depth",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American plaice fall or spring mean depth of occurrence (center of gravity). These data were sourced from NOAA's Distribution Mapping and Analysis Portal (DisMAP). These seasonal data are derived from the NMFS Regional Bottom Trawl Survey, and are calculated as biomass-weighted averages of depth, weighted by the interpolated biomass at each depth for each year (1974-2019) of the survey."
                                          ),
                                          accordionItem(
                                            title = "Plaice (season) Mean Latitude",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American plaice fall or spring mean latitude of occurrence (center of gravity). These data were sourced from NOAA's Distribution Mapping and Analysis Portal (DisMAP). These seasonal data are derived from the NMFS Regional Bottom Trawl Survey, and are calculated as biomass-weighted averages of latitude, weighted by the interpolated biomass at each latitude for each year (1974-2019) of the survey."
                                          ),
                                          accordionItem(
                                            title = "Plaice (season) R/SSB",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American Plaice estimated fall or spring mean recruits/SSB, denoted as recruit abundance in year t per SSB in year t-1."
                                          ),
                                          accordionItem(
                                            title = "Plaice (season) SSB NEFSC (kg/tow)",
                                            status = "primary",
                                            collapsed = FALSE,
                                            "American Plaice fall or spring SSB from NEFSC trawl survey. Units are in Albatross calibrated units of kg/tow."
                                          )
                                        )),
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
      } else if(length(c(input$variable, input$Stockdata)) == 2){ #plot 5 variables together
        fig1<- plot_ly(dataDf(), x = ~Year, y =~get(c(input$variable, input$Stockdata)[1]), 
                       type = 'scatter', mode = 'lines', name = paste(c(input$variable, input$Stockdata)[1]))
        fig2<- plot_ly(dataDf(), x = ~Year, y = ~get(c(input$variable, input$Stockdata)[2]), 
                       type = 'scatter', mode = 'lines',name = paste(c(input$variable, input$Stockdata)[2]))
        
        
        fig<-subplot(fig1, fig2, nrows = 2, shareX = TRUE) %>% 
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
    DT::datatable(tail(table2,n=second_col(table2)))
  })
  
  ################Dropdown for Plots page##### 
  output$Stockdata_selector = renderUI({ #creates Species select box object called in ui
    selectInput(inputId = "Stockdata", #name of input
                label = "Stock Data:", #label displayed in ui
                choices = names(Both%>%select(grep(input$plotSpecies, names(.), ignore.case = T))),
                #choices = as.character(unique(colnames(Both[24:41]))),
                # calls unique values from the State column in the previously created table
                selected = NULL,multiple=TRUE) #default choice (not required)
  })
  ################Dropdown for Data page####
  output$Stockdata_selector2 = renderUI({ #creates Species select box object called in ui
    selectInput(inputId = "Stockdata2", #name of input
                label = "Stock Data:", #label displayed in ui
                #choices = as.character(unique(stockData$Species)),
                choices = names(Both2[c(12:14,24:38)]%>%select(grep(input$Species, names(.), ignore.case = T))),
                # calls unique values from the State column in the previously created table
                selected = NULL,multiple=TRUE) #default choice (not required)
  })
  ##############
  observe(print(input$accordion1))
  observe(print(input$accordion2))
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
