########## ENVIRONMENTAL DATA###################################################################################
library(terra)
library(XML)
library(utils)
library(here)
library(dplyr)
library(tidyr)
#### SOURCE "automatic_ecodata.R"
source(here("Code/automatic_ecodata.R"))
big_ecodata<-lapply(ecodata_df,as.numeric)
####load salinity data from GLORYs####
salinity_data<-read.csv(here("Data/GLORYs_salinity/mean_salinity_GLORYs.csv"))
######### FISH DATA ###########################################################################################
####load in Striped Bass stock data from stock assessment reports####
stock_assess_data<-read.csv(here("Data/stock_assess_data2.csv"))
stock_assess_data<-replace(stock_assess_data,stock_assess_data=='',NA)
stock_assess_data<-sapply(stock_assess_data,as.character)
stock_assess_data<- as.data.frame(gsub(",","",stock_assess_data))
stock_assess_data<-lapply(stock_assess_data,as.numeric)
####load in Bluefin Tuna stock data from stock assessment reports####
bluefin_data<-readxl::read_excel(here("Data/Bluefin_stockdata.xlsx"))
bluefin_data<-bluefin_data[1:2] #only keep the first two columns
# Create the Year column and filter rows based on Label prefix
bluefin_data <- bluefin_data %>%
  mutate(Year = as.numeric(gsub("[^0-9]", "", Label))) %>%  # Extract year from the Label
  filter(Year >= 1950) %>%  # Keep only the years 1950 and above
  select(Year, Label, Value) %>%
  filter(grepl("^(SSB_|F_|Recr_)", Label)) %>%  # Keep only rows with Label starting with SSB_, F_, or Recr_
  mutate(Label = gsub("_[0-9]+$", "", Label))  # Remove the _year part from the Label

# Spread the data into separate columns
bluefin_data <- bluefin_data %>%
  pivot_wider(names_from = Label, values_from = Value) %>%
  rename(
    Bluefin_SSB_mt = SSB,
    Bluefin_F = F,
    Bluefin_Recruitment = Recr
  )
##### Load in American Plaice Data from stock assessment reports and Dismap ######
Plaice_data<-readxl::read_excel(here("Data/American_plaice.xlsx"))
####### load in DisMAP data######
source(here("Code/Dismap_data.R"))
#################################### Merge all dfs ##############################################################
Both <- list(big_ecodata, salinity_data,stock_assess_data,DisMAPdata,bluefin_data,Plaice_data)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)
Both <-Both[rowSums(is.na(Both)) != ncol(Both), ] #removes rows containing all NAs



#Both2<-as.data.frame(big_ecodata)
Both2<-as.data.frame(Both[c(1:19,31:34)]) #dismap herring data is being treated as ENV var and lobster data as fish data
Both2<-Both2[ , order(names(Both2))]
Both2 <- list(Both[1],Both2, stock_assess_data,DisMAPdata[c(1,2,3,8,9)],bluefin_data,Plaice_data)
Both2 <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both2)
Both2 <-Both2[rowSums(is.na(Both2)) != ncol(Both2), ] #removes rows containing all NAs
#the Both2 df is the same as "Both", but Env data is organized in alphabetical order
Both2 <-as.data.frame(lapply(Both2, function(x) { #make sure each column is classified as numeric instead of character
  as.numeric(as.character(x))
}))

uniquecolnames <- unique(gsub("_[^_]+$", "", names(Both2)[-1])) #get unique column names excluding year


rm(big_ecodata,DisMAPdata,ecodata_df,salinity_data,stock_assess_data)
