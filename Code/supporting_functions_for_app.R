library(dplyr)
library(plotly)
library(stats)
library(htmltools)
library(shiny)
library(graphics)
library(shinydashboardPlus)
library(shinydashboard)
library(DT)
library(terra)
library(utils)
###### tweaks, a list object to set up multiple-columns for checkboxGroupInput#####
tweaks <- list(
  tags$head(
    tags$style(HTML("
      .multicol { 
        height: 300px;
        -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
        -moz-column-count: 2;    /* Firefox */ 
        column-count: 2; 
        -moz-column-fill: auto;
        -column-fill: auto;
      }
      .fish-controls {margin-bottom: 20px; /* Bottom margin adjustment */}
    "))
  )
)


##### pick fish data: the checkboxes will control the data values plotted ######
fish_controls <- function(species, selected_vars, var_name) {
  filtered_columns <- grep(paste(species, collapse = "|"), colnames(Both2), value = TRUE)
  sorted_filtered_columns <- sort(filtered_columns)
  num_checkboxes <- length(sorted_filtered_columns)
  conditionNames<- gsub("_", " ", filtered_columns)
  conditionNames<- gsub("\\b(MAB|COG|ALL|GOM)\\b", "", conditionNames, ignore.case = TRUE)  # Remove specified words
  conditionNames<- gsub("^\\s+|\\s+$", "", conditionNames)  # Remove leading and trailing spaces
  conditionNames<- gsub("\\s+", " ", conditionNames) # Reduce multiple spaces to a single space
  conditionNames<- gsub("\\bLat\\b", "Latitude", conditionNames) #Write out Latitude fully
  conditionNames<- gsub("\\bCalfin anomaly\\b", "C. finmarchicus Anomaly", conditionNames) #Write out recognizable calanus abbreviation

  
  container_height <- ifelse(num_checkboxes <= 2, 45, 
                             ifelse(num_checkboxes %% 2 == 0, 
                                    45 + 25 * floor((num_checkboxes - 2) / 2), 
                                    45 + 25 * floor((num_checkboxes) / 2)))
  controls <- list(
    tags$div(
      align = 'left', 
      class = 'multicol', 
      style = paste0("border: 2px solid #E9E9E9; ", 
                     "border-radius: 10px; ", 
                     "padding: 10px; ", 
                     "height: ", container_height, "px;", 
                     "background-color: rgba(0, 0, 0, 0.2);"),  
      checkboxGroupInput(
        var_name, 
        label = NULL,
        choiceNames = sort(conditionNames, decreasing = FALSE),
        #choiceNames = sort(sorted_filtered_columns, decreasing = FALSE),
        choiceValues = sorted_filtered_columns,
        selected = sorted_filtered_columns[1]
      )
    ) 
  )
  
  return(controls)
}
########create sidebar: #########
sidebar <- dashboardSidebar(width = 150,
                            tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
                            sidebarMenu(
                              menuItem("Home", tabName = "Home", icon = icon("house")),
                              menuItem("Striped Bass", tabName = "StripedBass", icon = icon("fish")),
                              menuItem("Bluefin Tuna", tabName = "BluefinTuna",icon = icon("fish")),
                              menuItem("American Lobster", tabName = "AmericanLobster",icon = icon("fish")),
                              menuItem("Metadata", tabName = "Metadata",icon = icon("list"))
                              
                            )
                            #,
#                            HTML(paste0(
#                              "<div style='margin-top: 100px; text-align: center;'>",
#                              "<a href='https://github.com/Jamie-Behan' target='_blank'>", 
 #                             icon("github", class = "fa-3x"), 
#                              "<br/>Jamie Behan 2023",
#                              "</a>",
#                              "</div>"
#                            ))
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
########## Add header logos ###########
headerlogos<-    tags$li(
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
############ Home Tab #########
Hometab<-tabItem(
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
)#close tabitem
####### Species Landing page content #######
stripedbass_info<-tabPanel("Range & Info",
         fluidRow(
           column(width = 4,
                  HTML('<div style="width: 100%; height: 400px; background-color: black; margin-top: 30px;"></div>')),
           column(width = 8,
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
                  tags$li(HTML('<a href="C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/papers_writeups/StripedBassliteraturereview.pdf" style="font-size: 18px;" target="_blank">Environmental Effects on Striped Bass Stock Dynamics</a>')
                  ))))
BFT_info<-tabPanel("Range & Info",
                           fluidRow(
                             column(width = 4,
                                    HTML('<div style="width: 100%; height: 400px; background-color: black; margin-top: 30px;"></div>')),
                             column(width = 8,
                                    h3("Overview"),
                                    p("Atlantic bluefin tuna (Thunnus thynnus) are a large, migratory species, managed by ICCAT as east (Mediterranean spawning)
                                      and west (Gulf of Mexico spawning) stocks, with evidence of spawning activity in the Slope Sea off the Mid Atlantic Bight 
                                      (Richardson et al. 2016a, Richardson et al. 2016b, Hernandez et al. 2022).Climate change impacts, such as warming, acidification, 
                                      and changing ocean dynamics, are affecting their traditional spawning and feeding habitats, potentially leading to ecosystem 
                                      and population productivity shifts in the North Atlantic and Mediterranean Sea",
                                      style = "font-size: 18px;text-align: justify;"),
                                    h4("For more information, click the link below:", style = "padding-top: 40px;"),
                                    tags$li(HTML('<a href="C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/papers_writeups/BFTliteraturereview.pdf" style="font-size: 18px;" target="_blank">Environmental Effects on Bluefin Tuna Stock Dynamics</a>')
                                    ))))
AL_info<-tabPanel("Range & Info",
                   fluidRow(
                     column(width = 4,
                            HTML('<div style="width: 100%; height: 400px; background-color: black; margin-top: 30px;"></div>')),
                     column(width = 8,
                            h3("Overview"),
                            p("American Lobster (Homarus americanus) is a benthic crustacean whose range extends along the Atlantic coast from the Mid-Atlantic region of the US to 
                              Newfoundland, Canada (Waddy & Aiken, 1986), and supports one of the most lucrative fisheries in this region. In 2021, commercial landings reached a 
                              record-high value of 134 million pounds and an ex-vessel value of approximately $875 million (ASFMC, 2021). The Gulf of Maine (GOM) and Georges Bank 
                              (GB) stocks contribute over 90% of US lobster landings (ASMFC, 2020) and while these stocks are not currently overfished (ASMFC, 2020), environmental 
                              factors, such as water temperature, salinity, substrate type, depth, distance from shore, and the presence of shelter, influence the distribution and 
                              abundance of lobster populations. American lobsters have a recognized temperature preference between 12 and 18°C (Crossin et al., 1998) and a salinity 
                              preference between 20–32 ppt (Jury et al. 1994; Tanaka and Chen, 2015). Although substrate and habitat preferences vary by life stage, lobsters have 
                              been observed in diverse habitats including cobble, rock, mud, bedrock, sand, peat reefs, and eelgrass beds (Lawton and Lavalli, 1995).",
                              style = "font-size: 18px;text-align: justify;"),
                            h4("For more information, click the link below:", style = "padding-top: 40px;"),
                            tags$li(HTML('<a href="C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/papers_writeups/AMliteraturereview.pdf" style="font-size: 18px;" target="_blank">Environmental Effects on American Lobster Stock Dynamics</a>')
                            ))))
###### New DF to help classify data for use in creating appropriate plot Y-axis labels #######
# Create an empty dataframe with columns "Data_name" and "Data_type"
new_df <- data.frame(Data_name = character(0), Data_type = character(0), stringsAsFactors = FALSE)

# Iterate through each column in Both2 dataframe
for (col_name in colnames(Both2)) {
  data_values <- Both2[[col_name]]
  
  # Remove NA values from data
  filtered_data <- data_values[!is.na(data_values)]
  
  # Check if there are any remaining non-NA values
  if (length(filtered_data) > 0) {
    # Check conditions and classify the data type
    if (all(filtered_data >= -15 & filtered_data <= 15)) {
      data_type <- "Anomaly"
    } else if (grepl("Lat", col_name, ignore.case = TRUE)) {
      data_type <- "Latitude"
    } else if (all(filtered_data >= 0) && any(filtered_data > 1000)) {
      data_type <- "Biomass"
    } else if (grepl("Depth", col_name, ignore.case = TRUE)) {
      data_type <- "Depth"
    } else {
      data_type <- "Other"
    }
  } else {
    data_type <- "Other"  # If there are no valid values, classify as "Other"
  }
  # Extract Yname based on Data_name
  y_name <- gsub("_", " ", col_name)
  y_name <- gsub("\\b(MAB|COG|Mean|ALL|GOM|Annual)\\b", "", y_name, ignore.case = TRUE)  # Remove specified words
  y_name <- gsub("^\\s+|\\s+$", "", y_name)  # Remove leading and trailing spaces
  y_name <- gsub("\\s+", " ", y_name)  # Reduce multiple spaces to a single space
  
  # Modify Yname based on additional conditions for Yname2
  y_name2 <- gsub("\\b(Atlantic|River|American|Landings|Striped Bass|lobster|Biomass)\\b", "", y_name)
  y_name2 <- gsub("\\bAbundance\\b", "Abun.", y_name2)
  y_name2 <- gsub("\\bCommercial\\b", "Comm.", y_name2)
  y_name2 <- gsub("\\bRecreational\\b", "Rec.", y_name2)
  y_name2 <- gsub("\\bFall\\b", "FL", y_name2)
  y_name2 <- gsub("\\bSpring\\b", "SP", y_name2)
  y_name2 <- gsub("\\bCopepods\\b", "Copepod", y_name2)
  y_name2 <- gsub("\\bBottom Temp\\b", "bt", y_name2)
  y_name2 <- gsub("\\bAnomaly\\b", "", y_name2)
  y_name2 <- gsub("\\bAbsolute\\b", "Abs.", y_name2)
  y_name2 <- gsub("\\bLarge\\b", "Lg", y_name2)
  y_name2 <- gsub("\\bSmall\\b", "Sm", y_name2)
  y_name2 <- gsub("\\bherring\\b", "Herring", y_name2, ignore.case = TRUE)
  y_name2 <- gsub("^\\s+|\\s+$", "", y_name2)  # Remove leading and trailing spaces
  y_name2 <- gsub("\\s+", " ", y_name2)  # Reduce multiple spaces to a single space
  
  # Add a new row to the new dataframe
  new_df <- rbind(new_df, data.frame(Data_name = col_name, Data_type = data_type, Yname = y_name, Yname2 = y_name2, stringsAsFactors = FALSE))
}
# Create a named vector mapping Data_name to Yname
name_mapping <- setNames(new_df$Yname, new_df$Data_name)
name_mapping2 <- setNames(new_df$Yname2, new_df$Data_name)
################ Estimate trendlines function ###########
# Calculate trendlines for each variable based on dataDf()

################ plotly function for stacked/layered plots ####################
plot_function<-function(plottingstyle,num_variables=num_variables, dataDf=dataDf, all_vars=all_vars, name_mapping=name_mapping, name_mapping2=name_mapping2, show_trendline){
####supporting functions used throughout plot_function  
  data_subset <- dataDf() %>%
    filter(if_all(all_of(all_vars), ~ !is.na(.))) %>%
    select(Year, all_of(all_vars))
  trendlines <- lapply(all_vars, function(var) {
    if (show_trendline) {
      model <- lm(as.formula(paste0(var, " ~ Year")), data = dataDf(), na.action = na.exclude)
      fitted_values <- predict(model, newdata = data.frame(Year = dataDf()$Year))
      extended_trendline <- rep(NA, nrow(dataDf()))
      extended_trendline[!is.na(dataDf()[[var]])] <- fitted_values[!is.na(dataDf()[[var]])]
      extended_trendline
    } else {
      rep(NA, nrow(dataDf()))
    }
  })
####### start plot_function  
  if (plottingstyle == "Layered") {
    
    if (num_variables > 4){ #plot 5 variables together
      # Create a data frame for plotting trendlines
      trendline_data <- data.frame(Year = dataDf()$Year, 
                                   Trendline1 = trendlines[[1]],
                                   Trendline2 = trendlines[[2]],
                                   Trendline3 = trendlines[[3]],
                                   Trendline4 = trendlines[[4]],
                                   Trendline5 = trendlines[[5]])  #you have 5 variables
      plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
              type = 'scatter', mode = 'lines', name =name_mapping[all_vars[1]],
              line = list(color = "#00608A")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[2]],
                  line = list(color = "#ABB400")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[3]],
                  line = list(color = "#EA4F12")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[4]],
                  line = list(color = "#683562")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[5]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[5]],
                  line = list(color = "#FF7272")) %>%
        # Add trendlines if input$trendline is TRUE for the combined trendline_data
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline1,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[1]], "Trend"),
                  line = list(dash = 'dot',color = "#00608A")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline2,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[2]], "Trend"),
                  line = list(dash = 'dot',color = "#ABB400")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline3,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[3]], "Trend"),
                  line = list(dash = 'dot',color = "#EA4F12")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline4,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[4]], "Trend"),
                  line = list(dash = 'dot',color = "#683562")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline5,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[5]], "Trend"),
                  line = list(dash = 'dot',color = "#FF7272")) %>%
        layout(xaxis = list(title = "Year",
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE), 
               yaxis = list(title="",  
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE))
    } else if (num_variables == 4){ #if only 4 variables are chosen
      # Create a data frame for plotting trendlines
      trendline_data <- data.frame(Year = dataDf()$Year, 
                                   Trendline1 = trendlines[[1]],
                                   Trendline2 = trendlines[[2]],
                                   Trendline3 = trendlines[[3]],
                                   Trendline4 = trendlines[[4]])  #you have 4 variables
      plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
              type = 'scatter', mode = 'lines', name =name_mapping[all_vars[1]],
              line = list(color = "#00608A")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[2]],
                  line = list(color = "#ABB400")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[3]],
                  line = list(color = "#EA4F12")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[4]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[4]],
                  line = list(color = "#683562")) %>%
        # Add trendlines if input$trendline is TRUE for the combined trendline_data
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline1,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[1]], "Trend"),
                  line = list(dash = 'dot',color = "#00608A")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline2,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[2]], "Trend"),
                  line = list(dash = 'dot',color = "#ABB400")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline3,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[3]], "Trend"),
                  line = list(dash = 'dot',color = "#EA4F12")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline4,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[4]], "Trend"),
                  line = list(dash = 'dot',color = "#683562")) %>%
        layout(xaxis = list(title = "Year",
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE), 
               yaxis = list(title="",  
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE))
    } else if (num_variables == 3){ #if only 3 variables are chosen
      # Create a data frame for plotting trendlines
      trendline_data <- data.frame(Year = dataDf()$Year, 
                                   Trendline1 = trendlines[[1]],
                                   Trendline2 = trendlines[[2]],
                                   Trendline3 = trendlines[[3]])  #you have 3 variables
      plot_ly(dataDf(), x = ~Year, y =~get(all_vars[1]), 
              type = 'scatter', mode = 'lines', name =name_mapping[all_vars[1]],
              line = list(color = "#00608A")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[2]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[2]],
                  line = list(color = "#ABB400")) %>%
        add_trace(dataDf(), x = ~Year, y = ~get(all_vars[3]), 
                  type = 'scatter', mode = 'lines',name =name_mapping[all_vars[3]],
                  line = list(color = "#EA4F12")) %>%
        # Add trendlines if input$trendline is TRUE for the combined trendline_data
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline1,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[1]], "Trend"),
                  line = list(dash = 'dot',color = "#00608A")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline2,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[2]], "Trend"),
                  line = list(dash = 'dot',color = "#ABB400")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline3,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[3]], "Trend"),
                  line = list(dash = 'dot',color = "#EA4F12")) %>%
        layout(xaxis = list(title = "Year",
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE), 
               yaxis = list(title="",  
                            zerolinecolor = '#bdbdbd', 
                            zerolinewidth = 2,
                            showgrid = FALSE))
    } else if (num_variables > 1){ #if only 2 variables are chosen
      # Create a data frame for plotting trendlines
      trendline_data <- data.frame(Year = dataDf()$Year, 
                                   Trendline1 = trendlines[[1]],
                                   Trendline2 = trendlines[[2]])  #you have 2 variables
      
      plot_ly() %>%
        # Add traces for the selected variables from dataDf()
        add_trace(data = dataDf(), x = ~Year, y = ~get(all_vars[1]),
                  type = 'scatter', mode = 'lines', name = name_mapping[all_vars[1]],
                  line = list(color = "#00608A")) %>%
        add_trace(data = dataDf(), x = ~Year, y = ~get(all_vars[2]),
                  type = 'scatter', mode = 'lines', name = name_mapping[all_vars[2]],
      line = list(color = "#ABB400")) %>%
        # Add trendlines if input$trendline is TRUE for the combined trendline_data
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline1,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[1]], "Trend"),
      line = list(dash = 'dot',color = "#00608A")) %>%
        add_trace(data = trendline_data, x = ~Year, y = ~Trendline2,
                  type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[2]], "Trend"),
                  line = list(dash = 'dot',color = "#ABB400")) %>%
        layout(xaxis = list(title = "Year",
                            zerolinecolor = '#bdbdbd',
                            zerolinewidth = 2,
                            showgrid = FALSE),
               yaxis = list(title = "",
                            zerolinecolor = '#bdbdbd',
                            zerolinewidth = 2,
                            showgrid = FALSE))
  
    } else { #plot individually if only 1 is selected
      # Create a data frame for plotting trendlines
      trendline_data <- data.frame(Year = dataDf()$Year, 
                                   Trendline1 = trendlines[[1]])  #you have 1 variable
plot_ly() %>%
  # Add traces for the selected variables from dataDf()
  add_trace(data = dataDf(), x = ~Year, y = ~get(all_vars[1]),
            type = 'scatter', mode = 'lines', name = name_mapping[all_vars[1]],
            line = list(color = "#00608A")) %>%
  # Add trendlines if input$trendline is TRUE for the combined trendline_data
  add_trace(data = trendline_data, x = ~Year, y = ~Trendline1,
            type = 'scatter', mode = 'lines', name = paste(name_mapping[all_vars[1]], "Trend"),
            line = list(dash = 'dot',color = "#00608A")) %>%
  layout(xaxis = list(title = "Year",
                      zerolinecolor = '#bdbdbd',
                      zerolinewidth = 2,
                      showgrid = FALSE),
         yaxis = list(title = "",
                      zerolinecolor = '#bdbdbd',
                      zerolinewidth = 2,
                      showgrid = FALSE))

    }}#close if layered option
  else {

    plot_list <- lapply(1:num_variables, function(i) {
      # Use name_mapping to get the correct Yname for the variable
      y_name <- name_mapping[all_vars[i]]
      y_name2 <- name_mapping2[all_vars[i]]

      if (show_trendline) {
        trendline_model <- lm(as.formula(paste0(all_vars[i], " ~ Year")), data = dataDf(), na.action = na.exclude)
        trendline_values <- predict(trendline_model, newdata = data.frame(Year = dataDf()$Year))
      } else {
        trendline_values <- rep(NA, nrow(dataDf()))
      }
      
      plot_ly(dataDf(), x = ~Year, y = ~get(all_vars[i]), 
              type = 'scatter', mode = 'lines', name = y_name) %>%
        add_trace(x = ~Year, y = ~trendline_values,
                  type = 'scatter', mode = 'lines', name = paste(y_name, "Trend"),
        line = list(dash = 'dash',width = 0.5, color = "#000000")) %>%
        layout(yaxis = list(title = y_name2, side = 'left', showgrid = FALSE, zerolinecolor = '#bdbdbd', zerolinewidth = 1.5))
    })
    
    fig <- subplot(plot_list, nrows = num_variables, shareX = TRUE, titleY = TRUE, 
                   titleX = TRUE, margin = 0.03)
    
    layout(fig, xaxis = list(title = "Year", showgrid = FALSE),
           plot_bgcolor = '#e5ecf6')
  }}

################ function for datatable ####################
datatable_function<-function(all_varsY=all_varsY){

  table_subset <- Both2[, all_varsY, drop = FALSE]

  
  # Find the starting year based on the second oldest column
  start_year <- min(which(!is.na(table_subset$Year)))  # Initialize start year with the first non-NA year
  # Find the starting year of all data columns (excluding "Year")
  data_start_years <- sapply(all_varsY[-1], function(var) min(which(!is.na(table_subset[[var]]))))
  # If all data columns start at the same year, update start_year
  if (length(unique(data_start_years)) == 1) {
    start_year <- min(data_start_years)
  } else {
    # Find the second oldest column and update start_year if necessary
    start_year <- sort(unique(data_start_years))[2]}
  # Subset the data from the start_year to the end
  table_subset <- table_subset[start_year:nrow(table_subset), ]
  # Replace column names with prettier name labels
  colnames(table_subset) <- name_mapping[colnames(table_subset)]
  DT::datatable(table_subset,options = list(pageLength = 30))
  }