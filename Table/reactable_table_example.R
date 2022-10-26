library(pacman)
pacman::p_load(here,ecodata,knitr,dplyr,sparkline,reactable) 
here()

bottom_temp<-ecodata::bottom_temp
#bottom_temp<-bottom_temp[(bottom_temp$Var == "bottom temp anomaly in situ"), ]
bottom_temp<-bottom_temp[(bottom_temp$EPU == "GOM") & (bottom_temp$Var == "bottom temp anomaly in situ"), ]
bottom_temp<-bottom_temp[c(1,3)]
names(bottom_temp)[1] <- "Year"
names(bottom_temp)[2] <- "Anomalies"
#names(bottom_temp)[3] <- "EPU"
#plot(bottom_temp$Time,bottom_temp$Value)
#lines(bottom_temp$Time,bottom_temp$Value)
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
#data <- bottom_temp %>%
#  group_by(EPU) %>%
#  summarise(Anomalies= list(Anomalies)) %>%
#  mutate(Boxplot = NA, Timeseries = NA)

#reactable(data, columns = list(
#  Anomalies= colDef(cell = function(values) {
#    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(bottom_temp$Anomalies), width = 150, height = 45)
#  }),
#  Boxplot = colDef(cell = function(value, index) {
#    sparkline(data$Anomalies[[index]], type = "box", width = 150, height = 45)
#  }),
#  Timeseries = colDef(cell = function(value, index) {
#    sparkline(data$Anomalies[[index]], width = 150, height = 45)
#  })
#))


reactable(df,columns = list(
  Dataset = colDef(name = "Dataset"),
  Description = colDef(name = "Description"),
  Units = colDef(name = "Units"),
  Source = colDef(name = "Source"),
  Histogram = colDef(name = "Histogram",cell = function(values) {
    sparkline(hist$counts, type = "bar", chartRangeMin = 0, chartRangeMax = max(hist$counts), width = 200, height = 40)
  }),
  Bar_Plot= colDef(name="Bar Plot",cell = function(values) {
    sparkline(bottom_temp$Anomalies, type = "bar", chartRangeMin = 0, chartRangeMax = max(bottom_temp$Anomalies), width = 100, height = 40)
  }),
  Boxplot = colDef(name="Box Plot",cell= function(values) {
    sparkline(bottom_temp$Anomalies, type = "box", width = 100, height = 40)}),
  Timeseries = colDef(name="Timeseries",cell= function(values) {
    sparkline(bottom_temp$Anomalies,type="line",width = 100, height = 40)})
  )
)
