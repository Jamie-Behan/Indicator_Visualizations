#### Load R packages ######
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,plotly,DT,shinyjs,shinythemes,dplyr,stringr,readtext,XML,data.table,ecodata,shinyBS,huxtable,gridExtra,ggplot2,shinyWidgets,readxl,htmltools,knitr,shinydashboard,shinydashboardPlus,gmRi,here,install=TRUE)
here()
#### SOURCE "automatic_ecodata.R"
source(here("Code/automatic_ecodata.R"))
big_ecodata<-lapply(ecodata_df,as.numeric)
####load ecodata that has been combined into one big df from "making_big_ecodata_df.R"####
stock_assess_data<-read.csv(here("Data/stock_assess_data2.csv"))
stock_assess_data<-replace(stock_assess_data,stock_assess_data=='',NA)
stock_assess_data<-sapply(stock_assess_data,as.character)
stock_assess_data<- as.data.frame(gsub(",","",stock_assess_data))
stock_assess_data<-lapply(stock_assess_data,as.numeric)
#plaice_data<-read_excel(here("Data/Plaice_data_dependentvar.xlsx"))
##Merge all dfs
Both <- list(big_ecodata, stock_assess_data)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)
Both <-Both[rowSums(is.na(Both)) != ncol(Both), ] #removes rows containing all NAs

uniquecolnames <- unique(gsub("_[^_]+$", "", names(Both)[-1])) #get unique column names excluding year
#uniquecolnamesYEAR <- unique(gsub("_[^_]+$", "", names(Both))) #get unique column names including year

Both2<-as.data.frame(big_ecodata)
Both2<-Both2[ , order(names(Both2))]
Both2 <- list(Both[1],Both2, stock_assess_data)
Both2 <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both2)
Both2 <-Both2[rowSums(is.na(Both2)) != ncol(Both2), ] #removes rows containing all NAs

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
                                   choiceNames  = sort(gsub("_", " ", uniquecolnames[1:17]),decreasing = FALSE), ##replace_with space and select columns on env data only
                                   
                                   choiceValues = sort(colnames(Both[2:18]),decreasing = FALSE),  ##select columns on env data only
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
                                   choiceNames  = gsub("_", " ", colnames(Both2[1:18])), ##select columns on env data only
                                   
                                   choiceValues = colnames(Both2[1:18]), ##select columns on env data only 
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
  dashboardHeader(
    title = span("Visualizing Environmental Indicators in the Gulf of Maine", 
                 style = "color: #E9E9E9; font-size:34px;font-weight: bold;font-family: Arial"),
    titleWidth = 550,
    tags$li(
      a(href = 'https://www.gmri.org/',
        img(src = "https://github.com/gulfofmaine/gmRi/blob/master/inst/stylesheets/gmri_logo.png?raw=true",
            title = "gmri.org", height = "80px"),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown",
      tags$style(".main-header {max-height: 100px}"),
      tags$style(".main-header .logo {height: 100px}")
    )
  ),
  sidebar,
  dashboardBody(
    gmri_colors,
    fluidPage(
      tweaks,
      tabItems(
        tabItem(
          tabName = "Plots",
          h2("Choose Variables & Plot (up to 5 Total)"),
          sidebarPanel(
            controls,
            h3("Select Species and Stock Variables"),
            width = 12,
            selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass" = "Striped_Bass")),
            uiOutput("Stockdata_selector")
          ),
          radioButtons("Plotting_Style", "Select Plotting Style",
                       choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                       selected = "Layered"),
          mainPanel(
            width = 12,
            tabsetPanel(
              type = "tabs",
              tabPanel("Plot", plotlyOutput("plot"))
            )
          )
        ), #close tabitem
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
      
      
      layout(fig, xaxis = list(title = "Year"),
             plot_bgcolor = '#e5ecf6', 
             xaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis2 = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))}
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
                choices = names(Both2[19:25]%>%select(grep(input$Species, names(.), ignore.case = T))),
                selected = NULL,multiple=TRUE)
  })
  ##############
  observe(print(input$accordion1))
  observe(print(input$accordion2))
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
