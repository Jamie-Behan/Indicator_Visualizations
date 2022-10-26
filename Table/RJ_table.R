library(pacman)
pacman::p_load(here,ecodata,knitr,dplyr,sparkline,kableExtra) 
here()

bottom_temp<-ecodata::bottom_temp
bottom_temp<-bottom_temp[(bottom_temp$EPU == "GOM") & (bottom_temp$Var == "bottom temp anomaly in situ"), ]
bottom_temp<-bottom_temp[c(1,3)]
names(bottom_temp)[1] <- "Year"
names(bottom_temp)[2] <- "Anomalies"

plot(bottom_temp$Year,bottom_temp$Anomalies)
lines(bottom_temp$Year,bottom_temp$Anomalies)
hist<-hist(bottom_temp$Anomalies)
barplot(hist$density)
#########dataframe##############
df<-data.frame(Dataset="Bottom Temp Anomaly",
               Description= "In situ bottom temperature anomalmy values (1981-2010 reference period)",
               Units= "Degrees C",
               Source= "ecodata package in R",
               Histogram= "",
               Bar_Plot="",
               Boxplot="",
               Timeseries= "")




#########################################

df %>%
  kable(
    caption = "Row styling",
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position","striped"),
    stripe_color = "gray!10"
  )
