library(ecodata)
ecodata::nefs
test1<-as.data.frame(ecodata::nefsc_survey)
test1<-test1[test1$EPU == 'GOM',]
#test1<-test1[ which( test1$Var == "Planktivore Fall Biomass Index" | test1$Var =="Planktivore Spring Biomass Index") , ]
forage_spring<-test1[ which(test1$Var =="Planktivore Spring Biomass Index" & test1$Time >2008) , ]
forage_fall<-test1[ which(test1$Var =="Planktivore Fall Biomass Index"& test1$Time >2008) , ]
write.csv(forage_spring,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/forage_spring.csv", row.names = FALSE)
write.csv(forage_fall,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/forage_fall.csv", row.names = FALSE)
###Bottom Heatwave Dataset#####
bottom_heatwave_ecodata<-as.data.frame(ecodata::bottom_heatwave)
bottom_heatwave_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$EPU == 'GOM',]
bottom_heatwave_CI_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$Var == 'cumulative intensity',]
colnames(bottom_heatwave_CI_ecodata)[4] <- "bottom_heatwave_cumulative_intensity_deg_C"
bottom_heatwave_MI_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$Var == 'maximum intensity',]
colnames(bottom_heatwave_MI_ecodata)[4] <- "bottom_heatwave_maximum_intensity_deg_C"
bottom_heatwave_ecodata<-merge(bottom_heatwave_CI_ecodata[c(3,4)],bottom_heatwave_MI_ecodata[c(3,4)],by="Time",all=T)
##### Bottom Temp dataset#########
Bottom_Temp_ecodata<-as.data.frame(ecodata::bottom_temp)
Bottom_Temp_ecodata<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$EPU == 'GOM',]
bt_anomaly<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$Var == 'bottom temp anomaly in situ',]
colnames(bt_anomaly)[5] <- "bt_anomaly_deg_C"
sst_anomaly<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$Var == 'sst anomaly in situ',]
colnames(sst_anomaly)[5] <- "sst_anomaly_deg_C"

temperature_ecodata<-merge(bt_anomaly[c(1,5)],sst_anomaly[c(1,5)],by="Time",all=T)
temperature_ecodata<-merge(temperature_ecodata,bottom_heatwave_ecodata,by="Time",all=T)
###Bottom Temp Anomalies GLORYS dataset##########
Bottom_Temp_GLORYS_ecodata<-as.data.frame(ecodata::bottom_temp_glorys)
Bottom_Temp_GLORYS_ecodata<-Bottom_Temp_GLORYS_ecodata[Bottom_Temp_GLORYS_ecodata$EPU == 'GOM',]
bt_anomaly_GLORYS<-Bottom_Temp_GLORYS_ecodata[Bottom_Temp_GLORYS_ecodata$Var == 'GLORYS bottom temp anomaly',]
colnames(bt_anomaly_GLORYS)[5] <- "bt_anomaly_GLORYS_deg_C"
temperature_ecodata<-merge(temperature_ecodata,bt_anomaly_GLORYS[c(1,5)],by="Time",all=T)

rm(bottom_heatwave_CI_ecodata,bottom_heatwave_MI_ecodata,bottom_heatwave_ecodata,Bottom_Temp_ecodata,Bottom_Temp_GLORYS_ecodata,bt_anomaly,bt_anomaly_GLORYS,sst_anomaly)
#####Calanus abundance dataset#######
calanus_stage_ecodata<-as.data.frame(ecodata::calanus_stage)
calanus_stage_c5_spring<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Spring',]
colnames(calanus_stage_c5_spring)[5] <- "calanus_stage_c5_spring"
colnames(calanus_stage_c5_spring)[1] <- "Time"
calanus_stage_c5_summer<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Summer',]
colnames(calanus_stage_c5_summer)[5] <- "calanus_stage_c5_summer"
colnames(calanus_stage_c5_summer)[1] <- "Time"
calanus_stage_c5_fall<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Fall',]
colnames(calanus_stage_c5_fall)[5] <- "calanus_stage_c5_fall"
colnames(calanus_stage_c5_fall)[1] <- "Time"
    calanus_c5_all<-merge(calanus_stage_c5_spring[c(1,5)],calanus_stage_c5_summer[c(1,5)],by="Time",all=T)
    calanus_c5_all<-merge(calanus_c5_all,calanus_stage_c5_fall[c(1,5)],by="Time",all=T)

calanus_stage_adt_spring<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Spring',]
colnames(calanus_stage_adt_spring)[5] <- "calanus_stage_adt_spring"
colnames(calanus_stage_adt_spring)[1] <- "Time"
calanus_stage_adt_summer<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Summer',]
colnames(calanus_stage_adt_summer)[5] <- "calanus_stage_adt_summer"
colnames(calanus_stage_adt_summer)[1] <- "Time"
calanus_stage_adt_fall<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Fall',]
colnames(calanus_stage_adt_fall)[5] <- "calanus_stage_adt_fall"
colnames(calanus_stage_adt_fall)[1] <- "Time"
    calanus_adt_all<-merge(calanus_stage_adt_spring[c(1,5)],calanus_stage_adt_summer[c(1,5)],by="Time",all=T)
    calanus_adt_all<-merge(calanus_adt_all,calanus_stage_adt_fall[c(1,5)],by="Time",all=T)
    calanus_all<-merge(calanus_adt_all,calanus_c5_all,by="Time",all=T) ## merge c5 and adult data frames
temperature_ecodata<-merge(temperature_ecodata,calanus_all,by="Time",all=T) #update big df

rm(calanus_adt_all,calanus_all,calanus_c5_all,calanus_stage_adt_fall,calanus_stage_adt_spring,calanus_stage_adt_summer,calanus_stage_c5_fall,calanus_stage_c5_spring,calanus_stage_c5_summer,calanus_stage_ecodata)


#####ChlA primary Productivity dataset#####
##### decided not to include because earlierst data year is 1997. Plaice data starts in 1980
#chl_pp_ecodata<-as.data.frame(ecodata::chl_pp)
#chl_pp_ANNUAL_CHLOR_A_MEDIAN<-chl_pp_ecodata[chl_pp_ecodata$EPU == 'GOM' & chl_pp_ecodata$Var == 'ANNUAL_CHLOR_A_MEDIAN',]

#cold_pool_ecodata<-as.data.frame(ecodata::cold_pool) #Not using cold pool data because it only contains data from MAB

###GSI dataset######
gsi_ecodata<-as.data.frame(ecodata::gsi)
gsi_ecodata$Time <- substr(gsi$Time, 1, 4) 
gsi_ecodata<-aggregate(Value~Time,data=gsi_ecodata,FUN="mean")
colnames(gsi_ecodata)[2] <- "GSI_anomaly"
    temperature_ecodata<-merge(temperature_ecodata,gsi_ecodata,by="Time",all=T)

    ##### Heatwave data#########
heatwave_ecodata<-as.data.frame(ecodata::heatwave)
    heatwave_ecodata<-heatwave_ecodata[heatwave_ecodata$EPU == 'GOM',]
    heatwave_CI_ecodata<-heatwave_ecodata[heatwave_ecodata$Var == 'cumulative intensity',]
    colnames(heatwave_CI_ecodata)[3] <- "heatwave_cumulative_intensity_deg_C"
    heatwave_MI_ecodata<-heatwave_ecodata[heatwave_ecodata$Var == 'maximum intensity',]
    heatwave_MI_ecodata<-aggregate(Value~Time,data=heatwave_MI_ecodata,FUN="mean")
    colnames(heatwave_MI_ecodata)[2] <- "heatwave_maximum_intensity_deg_C"
    heatwave_ecodata<-merge(heatwave_CI_ecodata[c(1,3)],heatwave_MI_ecodata,by="Time",all=T)
  temperature_ecodata<-merge(temperature_ecodata,heatwave_ecodata,by="Time",all=T)   
 ####Longterm sst data####   
longterm_sst_ecodata<-as.data.frame(ecodata::long_term_sst)
  colnames(longterm_sst_ecodata)[2] <- "longterm_sst_deg_C"
  temperature_ecodata<-merge(temperature_ecodata,longterm_sst_ecodata[c(1,2)],by="Time",all=T)
  ####NAO dataset#######
nao_ecodata<-as.data.frame(ecodata::nao)
colnames(nao_ecodata)[2] <- "NAO_unitless"
temperature_ecodata<-merge(temperature_ecodata,nao_ecodata[c(1,2)],by="Time",all=T)
#####Seasonal OISST dataset####
seasonal_OISST_anom_ecodata<-as.data.frame(ecodata::seasonal_oisst_anom)
seasonal_OISST_anom_fall<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Fall OISST anomaly',]
colnames(seasonal_OISST_anom_fall)[1] <- "OISST_anom_fall"
seasonal_OISST_anom_spring<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Spring OISST anomaly',]
colnames(seasonal_OISST_anom_spring)[1] <- "OISST_anom_spring"
seasonal_OISST_anom_winter<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Winter OISST anomaly',]
colnames(seasonal_OISST_anom_winter)[1] <- "OISST_anom_winter"
seasonal_OISST_anom_summer<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Summer OISST anomaly',]
colnames(seasonal_OISST_anom_summer)[1] <- "OISST_anom_summer"
    seasonal_OISST<-merge(seasonal_OISST_anom_fall[c(1,4)],seasonal_OISST_anom_spring[c(1,4)],by="Time",all=T)
    seasonal_OISST<-merge(seasonal_OISST,seasonal_OISST_anom_summer[c(1,4)],by="Time",all=T)
    seasonal_OISST<-merge(seasonal_OISST,seasonal_OISST_anom_winter[c(1,4)],by="Time",all=T)
    temperature_ecodata<-merge(temperature_ecodata,seasonal_OISST,by="Time",all=T)
#####Slopewater dataset###  
slopewater_ecodata<-as.data.frame(ecodata::slopewater)
slopewater_ecodata_WSW<-slopewater_ecodata[slopewater_ecodata$Var=='WSW proportion ne channel',]
colnames(slopewater_ecodata_WSW)[3] <- "slopewater_WSW"
slopewater_ecodata_LSLW<-slopewater_ecodata[slopewater_ecodata$Var=='LSLW proportion ne channel',]
colnames(slopewater_ecodata_LSLW)[3] <- "slopewater_LSLW"
slopewater_all<-merge(slopewater_ecodata_WSW[c(1,3)],slopewater_ecodata_LSLW[c(1,3)],by="Time",all=T)
temperature_ecodata<-merge(temperature_ecodata,slopewater_all,by="Time",all=T)

big_ecodata<-temperature_ecodata
write.csv(big_ecodata,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/big_ecodata.csv", row.names = FALSE)  

library(ecodata)
ecodata::nefs
test1<-as.data.frame(ecodata::nefsc_survey)
test1<-test1[test1$EPU == 'GOM',]
#test1<-test1[ which( test1$Var == "Planktivore Fall Biomass Index" | test1$Var =="Planktivore Spring Biomass Index") , ]
forage_spring<-test1[ which(test1$Var =="Planktivore Spring Biomass Index" & test1$Time >2008) , ]
forage_fall<-test1[ which(test1$Var =="Planktivore Fall Biomass Index"& test1$Time >2008) , ]
write.csv(forage_spring,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/forage_spring.csv", row.names = FALSE)
write.csv(forage_fall,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/forage_fall.csv", row.names = FALSE)
###Bottom Heatwave Dataset#####
bottom_heatwave_ecodata<-as.data.frame(ecodata::bottom_heatwave)
bottom_heatwave_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$EPU == 'GOM',]
bottom_heatwave_CI_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$Var == 'cumulative intensity',]
colnames(bottom_heatwave_CI_ecodata)[4] <- "bottom_heatwave_cumulative_intensity_deg_C"
bottom_heatwave_MI_ecodata<-bottom_heatwave_ecodata[bottom_heatwave_ecodata$Var == 'maximum intensity',]
colnames(bottom_heatwave_MI_ecodata)[4] <- "bottom_heatwave_maximum_intensity_deg_C"
bottom_heatwave_ecodata<-merge(bottom_heatwave_CI_ecodata[c(3,4)],bottom_heatwave_MI_ecodata[c(3,4)],by="Time",all=T)
##### Bottom Temp dataset#########
Bottom_Temp_ecodata<-as.data.frame(ecodata::bottom_temp)
Bottom_Temp_ecodata<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$EPU == 'GOM',]
bt_anomaly<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$Var == 'bottom temp anomaly in situ',]
colnames(bt_anomaly)[5] <- "bt_anomaly_deg_C"
sst_anomaly<-Bottom_Temp_ecodata[Bottom_Temp_ecodata$Var == 'sst anomaly in situ',]
colnames(sst_anomaly)[5] <- "sst_anomaly_deg_C"

temperature_ecodata<-merge(bt_anomaly[c(1,5)],sst_anomaly[c(1,5)],by="Time",all=T)
temperature_ecodata<-merge(temperature_ecodata,bottom_heatwave_ecodata,by="Time",all=T)
###Bottom Temp Anomalies GLORYS dataset##########
Bottom_Temp_GLORYS_ecodata<-as.data.frame(ecodata::bottom_temp_glorys)
Bottom_Temp_GLORYS_ecodata<-Bottom_Temp_GLORYS_ecodata[Bottom_Temp_GLORYS_ecodata$EPU == 'GOM',]
bt_anomaly_GLORYS<-Bottom_Temp_GLORYS_ecodata[Bottom_Temp_GLORYS_ecodata$Var == 'GLORYS bottom temp anomaly',]
colnames(bt_anomaly_GLORYS)[5] <- "bt_anomaly_GLORYS_deg_C"
temperature_ecodata<-merge(temperature_ecodata,bt_anomaly_GLORYS[c(1,5)],by="Time",all=T)

rm(bottom_heatwave_CI_ecodata,bottom_heatwave_MI_ecodata,bottom_heatwave_ecodata,Bottom_Temp_ecodata,Bottom_Temp_GLORYS_ecodata,bt_anomaly,bt_anomaly_GLORYS,sst_anomaly)
#####Calanus abundance dataset#######
calanus_stage_ecodata<-as.data.frame(ecodata::calanus_stage)
calanus_stage_c5_spring<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Spring',]
colnames(calanus_stage_c5_spring)[5] <- "calanus_stage_c5_spring"
colnames(calanus_stage_c5_spring)[1] <- "Time"
calanus_stage_c5_summer<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Summer',]
colnames(calanus_stage_c5_summer)[5] <- "calanus_stage_c5_summer"
colnames(calanus_stage_c5_summer)[1] <- "Time"
calanus_stage_c5_fall<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'c5'& calanus_stage_ecodata$season == 'Fall',]
colnames(calanus_stage_c5_fall)[5] <- "calanus_stage_c5_fall"
colnames(calanus_stage_c5_fall)[1] <- "Time"
    calanus_c5_all<-merge(calanus_stage_c5_spring[c(1,5)],calanus_stage_c5_summer[c(1,5)],by="Time",all=T)
    calanus_c5_all<-merge(calanus_c5_all,calanus_stage_c5_fall[c(1,5)],by="Time",all=T)

calanus_stage_adt_spring<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Spring',]
colnames(calanus_stage_adt_spring)[5] <- "calanus_stage_adt_spring"
colnames(calanus_stage_adt_spring)[1] <- "Time"
calanus_stage_adt_summer<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Summer',]
colnames(calanus_stage_adt_summer)[5] <- "calanus_stage_adt_summer"
colnames(calanus_stage_adt_summer)[1] <- "Time"
calanus_stage_adt_fall<-calanus_stage_ecodata[calanus_stage_ecodata$EPU == 'GOM'& calanus_stage_ecodata$Var == 'adt'& calanus_stage_ecodata$season == 'Fall',]
colnames(calanus_stage_adt_fall)[5] <- "calanus_stage_adt_fall"
colnames(calanus_stage_adt_fall)[1] <- "Time"
    calanus_adt_all<-merge(calanus_stage_adt_spring[c(1,5)],calanus_stage_adt_summer[c(1,5)],by="Time",all=T)
    calanus_adt_all<-merge(calanus_adt_all,calanus_stage_adt_fall[c(1,5)],by="Time",all=T)
    calanus_all<-merge(calanus_adt_all,calanus_c5_all,by="Time",all=T) ## merge c5 and adult data frames
temperature_ecodata<-merge(temperature_ecodata,calanus_all,by="Time",all=T) #update big df

rm(calanus_adt_all,calanus_all,calanus_c5_all,calanus_stage_adt_fall,calanus_stage_adt_spring,calanus_stage_adt_summer,calanus_stage_c5_fall,calanus_stage_c5_spring,calanus_stage_c5_summer,calanus_stage_ecodata)


#####ChlA primary Productivity dataset#####
##### decided not to include because earlierst data year is 1997. Plaice data starts in 1980
#chl_pp_ecodata<-as.data.frame(ecodata::chl_pp)
#chl_pp_ANNUAL_CHLOR_A_MEDIAN<-chl_pp_ecodata[chl_pp_ecodata$EPU == 'GOM' & chl_pp_ecodata$Var == 'ANNUAL_CHLOR_A_MEDIAN',]

#cold_pool_ecodata<-as.data.frame(ecodata::cold_pool) #Not using cold pool data because it only contains data from MAB

###GSI dataset######
gsi_ecodata<-as.data.frame(ecodata::gsi)
gsi_ecodata$Time <- substr(gsi$Time, 1, 4) 
gsi_ecodata<-aggregate(Value~Time,data=gsi_ecodata,FUN="mean")
colnames(gsi_ecodata)[2] <- "GSI_anomaly"
    temperature_ecodata<-merge(temperature_ecodata,gsi_ecodata,by="Time",all=T)

    ##### Heatwave data#########
heatwave_ecodata<-as.data.frame(ecodata::heatwave)
    heatwave_ecodata<-heatwave_ecodata[heatwave_ecodata$EPU == 'GOM',]
    heatwave_CI_ecodata<-heatwave_ecodata[heatwave_ecodata$Var == 'cumulative intensity',]
    colnames(heatwave_CI_ecodata)[3] <- "heatwave_cumulative_intensity_deg_C"
    heatwave_MI_ecodata<-heatwave_ecodata[heatwave_ecodata$Var == 'maximum intensity',]
    heatwave_MI_ecodata<-aggregate(Value~Time,data=heatwave_MI_ecodata,FUN="mean")
    colnames(heatwave_MI_ecodata)[2] <- "heatwave_maximum_intensity_deg_C"
    heatwave_ecodata<-merge(heatwave_CI_ecodata[c(1,3)],heatwave_MI_ecodata,by="Time",all=T)
  temperature_ecodata<-merge(temperature_ecodata,heatwave_ecodata,by="Time",all=T)   
 ####Longterm sst data####   
longterm_sst_ecodata<-as.data.frame(ecodata::long_term_sst)
  colnames(longterm_sst_ecodata)[2] <- "longterm_sst_deg_C"
  temperature_ecodata<-merge(temperature_ecodata,longterm_sst_ecodata[c(1,2)],by="Time",all=T)
  ####NAO dataset#######
nao_ecodata<-as.data.frame(ecodata::nao)
colnames(nao_ecodata)[2] <- "NAO_unitless"
temperature_ecodata<-merge(temperature_ecodata,nao_ecodata[c(1,2)],by="Time",all=T)
#####Seasonal OISST dataset####
seasonal_OISST_anom_ecodata<-as.data.frame(ecodata::seasonal_oisst_anom)
seasonal_OISST_anom_fall<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Fall OISST anomaly',]
colnames(seasonal_OISST_anom_fall)[1] <- "OISST_anom_fall"
seasonal_OISST_anom_spring<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Spring OISST anomaly',]
colnames(seasonal_OISST_anom_spring)[1] <- "OISST_anom_spring"
seasonal_OISST_anom_winter<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Winter OISST anomaly',]
colnames(seasonal_OISST_anom_winter)[1] <- "OISST_anom_winter"
seasonal_OISST_anom_summer<-seasonal_OISST_anom_ecodata[seasonal_OISST_anom_ecodata$EPU == 'GOM'& seasonal_OISST_anom_ecodata$Var=='Summer OISST anomaly',]
colnames(seasonal_OISST_anom_summer)[1] <- "OISST_anom_summer"
    seasonal_OISST<-merge(seasonal_OISST_anom_fall[c(1,4)],seasonal_OISST_anom_spring[c(1,4)],by="Time",all=T)
    seasonal_OISST<-merge(seasonal_OISST,seasonal_OISST_anom_summer[c(1,4)],by="Time",all=T)
    seasonal_OISST<-merge(seasonal_OISST,seasonal_OISST_anom_winter[c(1,4)],by="Time",all=T)
    temperature_ecodata<-merge(temperature_ecodata,seasonal_OISST,by="Time",all=T)
#####Slopewater dataset###  
slopewater_ecodata<-as.data.frame(ecodata::slopewater)
slopewater_ecodata_WSW<-slopewater_ecodata[slopewater_ecodata$Var=='WSW proportion ne channel',]
colnames(slopewater_ecodata_WSW)[3] <- "slopewater_WSW"
slopewater_ecodata_LSLW<-slopewater_ecodata[slopewater_ecodata$Var=='LSLW proportion ne channel',]
colnames(slopewater_ecodata_LSLW)[3] <- "slopewater_LSLW"
slopewater_all<-merge(slopewater_ecodata_WSW[c(1,3)],slopewater_ecodata_LSLW[c(1,3)],by="Time",all=T)
temperature_ecodata<-merge(temperature_ecodata,slopewater_all,by="Time",all=T)

big_ecodata<-temperature_ecodata
write.csv(big_ecodata,"C:/Users/jbehan/Box/Jamie Behan/GMRI/Openscapes/Indicator_visualizations/big_ecodata.csv", row.names = FALSE)  