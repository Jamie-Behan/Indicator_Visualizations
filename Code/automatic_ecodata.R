if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(ecodata, dplyr,reshape2,XML,stringr)
##### Bottom Temp & sst anomaly dataset#########
ecodata_df<-as.data.frame(ecodata::bottom_temp[c(1:4)])
ecodata_df<-ecodata_df[ecodata_df$Var == 'bottom temp anomaly in situ'| ecodata_df$Var == 'sst anomaly in situ',]
ecodata_df$Var[ecodata_df$Var == 'bottom temp anomaly in situ'] <- 'Bottom_Temp_Anomaly'
ecodata_df$Var[ecodata_df$Var == 'sst anomaly in situ'] <- 'SST_Temp_Anomaly'

##### Bottom Temp raw dataset#########
other_df<-as.data.frame(ecodata::bottom_temp_comp[c(1:4)])
other_df<-other_df[other_df$Var == 'Annual_Bottom Temp',]
other_df$Var <- paste0("Annual_Bottom_Temp_Absolute")

ecodata_df<-rbind(ecodata_df,other_df)
###Bottom Temp Anomalies GLORYS dataset##########
other_df<-as.data.frame(ecodata::bottom_temp_glorys[c(1:3,5)])
other_df<-other_df[other_df$Var == 'GLORYS bottom temp anomaly',]
other_df$Var <- paste0("GLORYS_Bottom_Temp_Anomaly")

ecodata_df<-rbind(ecodata_df,other_df)
#####zooplankton abundance anomaly dataset#######
other_df<-as.data.frame(ecodata::zoo_abundance_anom[c(1:4)])
other_df<-other_df[other_df$Var == 'Calfin'|other_df$Var == 'SmCopepods'|other_df$Var == 'LgCopepods',]
other_df$Var[other_df$Var == 'Calfin'] <- 'Calfin_anomaly'
other_df$Var[other_df$Var == 'SmCopepods'] <- 'Small_Copepods_Abundance_Anomaly'
other_df$Var[other_df$Var == 'LgCopepods'] <- 'Large_Copepods_Abundance_Anomaly'

ecodata_df<-rbind(ecodata_df,other_df)
###GSI dataset######
other_df<-as.data.frame(ecodata::gsi)
other_df$Time <- substr(gsi$Time, 1, 4) 
other_df<-distinct(data.frame(other_df[c(2,4)],aggregate(Value~Time,data=other_df,FUN="mean")))
other_df$Var <- paste0("GSI")

ecodata_df<-rbind(ecodata_df,other_df)
####NAO dataset#######
other_df<-as.data.frame(ecodata::nao[c(1:4)])
other_df$Var <- paste0("NAO")

ecodata_df<-rbind(ecodata_df,other_df)
#####Seasonal OISST dataset####
other_df<-as.data.frame(ecodata::seasonal_oisst_anom)
other_df$Var <- paste0(other_df$Var,"_OISST")
ecodata_df<-rbind(ecodata_df,other_df)
#### forage fish dataset ####
other_df<-as.data.frame(ecodata::forage_index)
other_df<-other_df[other_df$Var == 'Annual Forage Fish Biomass Estimate'& other_df$EPU == 'GOM',]
other_df<-other_df[,c(-5)]
other_df$Var <- paste0("Annual_Forage_Fish_Biomass")
ecodata_df<-rbind(ecodata_df,other_df)
#### Chlorophyll A dataset ####
other_df<-as.data.frame(ecodata::chl_pp)
other_df<-other_df[other_df$Var == 'MONTHLY_CHLOR_A_MEDIAN'& other_df$EPU == 'GOM',]
other_df<-other_df[,c(-5)]
other_df$Time<-str_sub(other_df$Time, start=3,end=6)
other_df$Time<-as.numeric(other_df$Time)
other_df<-distinct(data.frame(other_df[c(2,4)],aggregate(Value~Time,data=other_df,FUN="mean")))
other_df<-other_df[,c(3,1,4,2)]
other_df$Var <- paste0("Annual_ChlorA") 
ecodata_df<-rbind(ecodata_df,other_df)
#####hudson river flow dataset#### 
other_df<-as.data.frame(ecodata::hudson_river_flow)
other_df$Var <- paste0("Hudson_River_Flow_Rate")
hudson_river<-other_df
ecodata_df<-rbind(ecodata_df,other_df)
#### AMO data from https://psl.noaa.gov/data/timeseries/AMO/ (using short unsmoothed data)####
url<-"https://psl.noaa.gov/data/correlation/amon.us.data"
amo_data = read.csv(url, header=FALSE)
amo_data<- as.data.frame(amo_data[-c(1), ])
amo_data<-as.data.frame(head(amo_data,-6))
names(amo_data)[1] <- "AMO"
amo_data$AMO<-as.character(amo_data$AMO)
amo_data[c('Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')] <- str_split_fixed(amo_data$AMO,'   ',13)# Split name column into mulitple columns by month
amo_data<-amo_data[,-1] #remove first col
amo_data<-as.data.frame(lapply(amo_data,as.numeric)) #convert from character class to numeric
amo_data$AMO_Annual_Mean<-rowMeans(amo_data[ ,2:13]) ###find annual average
amo_data<-amo_data[,-c(2:13)] #remove all columns except year and annual mean columns
####### combine everything into one df #####
ecodata_df<-ecodata_df%>% filter(Time >= "1960")
hudson_river<-hudson_river%>% filter(Time >= "1960")
names(ecodata_df)[1] <- "Year"
names(hudson_river)[1] <- "Year"
GOM_ecodata <-ecodata_df[ecodata_df$EPU == 'GOM',]
ALL_ecodata <-ecodata_df[ecodata_df$EPU == 'All',]
hudson_river <-hudson_river[hudson_river$EPU == 'MAB',]
rm(ecodata_df,other_df)


GOM_ecodata <-reshape2::dcast(GOM_ecodata,Year~Var,value.var="Value")
ALL_ecodata <-reshape2::dcast(ALL_ecodata,Year~Var,value.var="Value")
hudson_river <-reshape2::dcast(hudson_river,Year~Var,value.var="Value")#hudson riverflow data

colnames(GOM_ecodata)<- paste(colnames(GOM_ecodata),"GOM",sep="_")
colnames(ALL_ecodata)<- paste(colnames(ALL_ecodata),"ALL",sep="_")
colnames(hudson_river)<- paste(colnames(hudson_river),"MAB",sep="_") #hudson riverflow data

names(GOM_ecodata)[1] <- "Year"
names(ALL_ecodata)[1] <- "Year"
names(hudson_river)[1] <- "Year" #hudson riverflow data

#put all data frames into list
#df_list <- list(GOM_ecodata,GB_ecodata,MAB_ecodata,SS_ecodata,ALL_ecodata)
df_list <- list(GOM_ecodata,ALL_ecodata,hudson_river)
#merge all data frames in list
ecodata_df<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
ecodata_df<-merge(ecodata_df,amo_data,by="Year")
#rm(df_list,GOM_ecodata,GB_ecodata,MAB_ecodata,SS_ecodata,ALL_ecodata,hudson_river)
rm(df_list,GOM_ecodata,ALL_ecodata,hudson_river,amo_data)
