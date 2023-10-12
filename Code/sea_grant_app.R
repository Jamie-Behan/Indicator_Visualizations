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
  dashboardHeader(
    titleWidth = 150,
    tags$li(
      div(
        style = "display: flex; align-items: center;",
        a(href = 'https://umaine.edu/marine/',
          img(src = "https://i.pinimg.com/originals/a6/56/72/a656729123a2857f93010c5a9aa70c0d.png",
              title = "Umaine School of Marine Science", height = "80px"),
          style = "padding-top: 10px; padding-bottom: 10px;"
        ),
        style = "display: flex; align-items: center;",
        a(href = 'https://www.gmri.org/',
          img(src = "https://github.com/gulfofmaine/gmRi/blob/master/inst/stylesheets/gmri_logo.png?raw=true",
              title = "gmri.org", height = "80px"),
          style = "padding-top: 10px; padding-bottom: 10px; margin-left: 20px;"
        ),
        a(href = 'https://www.maine.gov/dmr/home',
          img(src = "https://d3esu6nj4wau0q.cloudfront.net/images/MaineDMR_logo.width-460.png",
              title = "Maine DMR Website", height = "80px"),
          style = "padding-top: 10px; padding-bottom: 10px; margin-left: 20px;"  # Adjust margin-left as needed
        )
      ),
      class = "dropdown",
      tags$style(".main-header {max-height: 100px}"),
      tags$style(".main-header .logo {height: 100px}")
    )
  )
  ,
  sidebar,
  dashboardBody(
    gmri_colors,
    fluidPage(
      tweaks,
      tabItems(
        tabItem(
          tabName = "Home",
          fluidPage(
            tags$style(HTML("
      .home-background {
        background-color: #00736D;
        height: 100vh;
        margin: 0;
        display: flex;
        flex-direction: column;
        justify-content: flex-start; /* Align content to the top */
        align-items: center;
        border-radius: 35px;
        padding-top: 35px; /* Add a small buffer (adjust the value according to your preference) */
        position: relative; /* Set position to relative for proper placement of the image */
      }
      .overlay {
        position: absolute;
        top: 0;
        left: 25;
        width: 90%;
        height: 40%;
        background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent black background */
        border-radius: 35px; /* Rounded corners for the overlay */
        z-index: 1; /* Place overlay behind the text */
      }
      .home-content {
        text-align: center;
        color: white;
        max-width: 850px;
        z-index: 2; /* Ensure text is above the overlay */
      }
      .boat-image {
        position: absolute; /* Position the image absolutely within the .home-background */
        top: -20px; /* Distance from the top (adjust as needed) */
        left: 50%; /* Center the image horizontally */
        transform: translateX(-50%); /* Center the image horizontally */
        border-radius: 35px; /* Rounded corners for the image */
        z-index: 0; /* Place image behind the text and overlay */
      }
    ")),
            div(class = "home-background",
                img(src = "coastal-excursion-aerial.jpg", class = "boat-image", width = "2100px"),  # Add the boat image here
                div(class = "overlay"),  # Add overlay here
                div(class = "home-content",
                    tags$h1("Monitoring Ecosystem Change to Support Fisheries Decision-Making in Maine's Coastal Waters", style = "font-size: 36px;"),
                    tags$p("Warming in Maine’s coastal waters is reshaping the ecosystem and impacting key fishery resources and communities. Information on the state of the ecosystem will improve our ability to make informed decisions in the face of climate change and support a holistic, ecosystem-based approach to managing Maine’s marine resources. The goal of this study is to develop an integrated ecosystem assessment (IEA) focused on characterizing the status and trends of the fishery ecosystem in Maine’s coastal waters.", style = "font-size: 18px;")
                )
            )
          )
        )
        
        , #close tabitem
        
        
        tabItem(
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
                h2("Choose Variables & Plot (up to 5 Total)"),
                fluidRow(
                  column(
                    width = 12,
                    h3("Stock Variables"),
                    selectInput("plotSpecies", "plotSpecies", choices = c("Striped Bass" = "Striped_Bass")),
                    uiOutput("Stockdata_selector")
                  ),
                  column(
                    width = 6,
                    controls,
                    radioButtons(
                      "Plotting_Style",
                      "Select Plotting Style",
                      choices = c("Layered" = "Layered", "Stacked" = "Stacked"),
                      selected = "Stacked"
                    )
                  )
                ),
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
