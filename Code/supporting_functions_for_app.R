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
        choiceNames = sort(gsub("_", " ", sorted_filtered_columns), decreasing = FALSE),
        choiceValues = sorted_filtered_columns,
        selected = sorted_filtered_columns[1]
      )
    ) 
  )
  
  return(controls)
}
##### controls2: year column included######
controls2 <-
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("show_vars", 
                                   label = NULL, 
                                   choiceNames  = gsub("_", " ", colnames(Both2[1:23])), ##select columns on env data only
                                   
                                   choiceValues = colnames(Both2[1:23]), ##select columns on env data only 
                                   selected = c("Year","Bottom_Temp_Anomaly_GOM",multiple = TRUE))
       )#close tags$div
  )#close list 
###### finding name of second longest column (excluding NAs) ##############
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
                              menuItem("Home", tabName = "Home", icon = icon("house")),
                              menuItem("Striped Bass", tabName = "StripedBass", icon = icon("fish")),
                              menuItem("Bluefin Tuna", tabName = "BluefinTuna",icon = icon("fish")),
                              menuItem("American Lobster", tabName = "AmericanLobster",icon = icon("fish")),
                              menuItem("Metadata", tabName = "Metadata",icon = icon("list"))
                              
                            ),
                            HTML(paste0(
                              "<div style='margin-top: 100px; text-align: center;'>",
                              "<a href='https://github.com/Jamie-Behan' target='_blank'>", 
                              icon("github", class = "fa-3x"), 
                              "<br/>Jamie Behan 2023",
                              "</a>",
                              "</div>"
                            ))
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
####### Landing page content
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
  y_name <- gsub("\\b(MAB|COG|Mean|ALL|GOM|Annual|Absolute|Anomaly)\\b", "", y_name, ignore.case = TRUE)  # Remove specified words
  
  # Modify Yname based on additional conditions for Yname2
  y_name2 <- gsub("\\b(Atlantic|River|American|Landings|Striped Bass|lobster)\\b", "", y_name)
  y_name2 <- gsub("\\bAbundance\\b", "Abun.", y_name2)
  y_name2 <- gsub("\\bCommercial\\b", "Comm.", y_name2)
  y_name2 <- gsub("\\bRecreational\\b", "Rec.", y_name2)
  y_name2 <- gsub("\\bFall\\b", "FL", y_name2)
  y_name2 <- gsub("\\bSpring\\b", "SP", y_name2)
  y_name2 <- gsub("\\bherring\\b", "Herring", y_name2, ignore.case = TRUE)
  # Remove leading and trailing spaces, and reduce multiple spaces to a single space
  y_name2 <- gsub("^\\s+|\\s+$", "", y_name2)  # Remove leading and trailing spaces
  y_name2 <- gsub("\\s+", " ", y_name2)  # Reduce multiple spaces to a single space
  
  # Add a new row to the new dataframe
  new_df <- rbind(new_df, data.frame(Data_name = col_name, Data_type = data_type, Yname = y_name, Yname2 = y_name2, stringsAsFactors = FALSE))
}
# Create a named vector mapping Data_name to Yname
name_mapping <- setNames(new_df$Yname, new_df$Data_name)
name_mapping2 <- setNames(new_df$Yname2, new_df$Data_name)
################ Create a new dataframe "Both3" by rounding the values in "Both2" to 2 decimal places ####################
Both3 <- as.data.frame(lapply(Both2, function(x) {
  as.numeric(as.character(x))
}))
# Round all columns to 2 decimal places
Both3[] <- lapply(Both3, function(x) round(x, 2))
