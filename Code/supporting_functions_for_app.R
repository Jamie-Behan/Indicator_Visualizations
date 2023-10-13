###### tweaks, a list object to set up multiple-columns for checkboxGroupInput#####
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
  list( 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("variable", 
                                   label = NULL, 
                                   choiceNames  = sort(gsub("_", " ", uniquecolnames[c(1:22)]),decreasing = FALSE), ##replace_with space and select columns on env data only
                                   
                                   choiceValues = sort(colnames(Both2[2:23]),decreasing = FALSE),  ##select columns on env data only
                                   selected = c("Bottom_Temp_Anomaly_GOM",multiple = TRUE)) 
                
       )#close tags$div
  )#close list 
##### pick fish data: the checkboxes will control the data values plotted ######
fish_controls <- function(species) {
  filtered_columns <- grep(paste(species, collapse="|"), colnames(Both2), value = TRUE)
  sorted_filtered_columns <- sort(filtered_columns)
  
  controls <- list(
    tags$div(align = 'left', 
             class = 'multicol', 
             checkboxGroupInput("variable", 
                                label = NULL, 
                                choiceNames  = sort(gsub("_", " ", sorted_filtered_columns), decreasing = FALSE),
                                choiceValues = sorted_filtered_columns,
                                selected = sorted_filtered_columns[1])
    ) # close tags$div
  ) # close list
  
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
