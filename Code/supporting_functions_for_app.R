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
  list(h3("Select Environmental Variables"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput("variable", 
                                   label = NULL, 
                                   choiceNames  = sort(gsub("_", " ", uniquecolnames[c(1:18,29:30)]),decreasing = FALSE), ##replace_with space and select columns on env data only
                                   
                                   choiceValues = sort(colnames(Both2[2:23]),decreasing = FALSE),  ##select columns on env data only
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