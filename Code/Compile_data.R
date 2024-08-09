########## ENVIRONMENTAL DATA###################################################################################
library(terra)
library(XML)
library(utils)
library(here)
#### SOURCE "automatic_ecodata.R"
source(here("Code/automatic_ecodata.R"))
big_ecodata<-lapply(ecodata_df,as.numeric)
####load salinity data from GLORYs####
salinity_data<-read.csv(here("Data/GLORYs_salinity/mean_salinity_GLORYs.csv"))
######### FISH DATA ###########################################################################################
####load in stock data from stock assessment reports####
stock_assess_data<-read.csv(here("Data/stock_assess_data2.csv"))
stock_assess_data<-replace(stock_assess_data,stock_assess_data=='',NA)
stock_assess_data<-sapply(stock_assess_data,as.character)
stock_assess_data<- as.data.frame(gsub(",","",stock_assess_data))
stock_assess_data<-lapply(stock_assess_data,as.numeric)
####### load in DisMAP data######
source(here("Code/Dismap_data.R"))
#################################### Merge all dfs ##############################################################
Both <- list(big_ecodata, salinity_data,stock_assess_data,DisMAPdata)
Both <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both)
Both <-Both[rowSums(is.na(Both)) != ncol(Both), ] #removes rows containing all NAs



#Both2<-as.data.frame(big_ecodata)
Both2<-as.data.frame(Both[c(1:19,31:34)]) #dismap herring data is being treated as ENV var and lobster data as fish data
Both2<-Both2[ , order(names(Both2))]
Both2 <- list(Both[1],Both2, stock_assess_data,DisMAPdata[c(1,2,3,8,9)])
Both2 <-Reduce(function(x, y) merge(x, y, by="Year",all=T), Both2)
Both2 <-Both2[rowSums(is.na(Both2)) != ncol(Both2), ] #removes rows containing all NAs
#the Both2 df is the same as "Both", but is organized in alphabetical order
Both2 <-as.data.frame(lapply(Both2, function(x) { #make sure each column is classified as numeric instead of character
  as.numeric(as.character(x))
}))

uniquecolnames <- unique(gsub("_[^_]+$", "", names(Both2)[-1])) #get unique column names excluding year


rm(big_ecodata,DisMAPdata,ecodata_df,salinity_data,stock_assess_data)
