#This script was run to create the "mean_salinity_GLORYs.csv" data file, which is used in sea_grant_app.R as the salinity timeseries
library(pacman)
#pacman::p_load(readxl,sdm,raster,sf,ncdf4,maptools,rgdal,maps,mapdata,dplyr,viridis,sp,rgeos,writexl,RColorBrewer,classInt,raster,akima)
pacman::p_load(terra,graphics,dplyr,utils,install=TRUE)

dat <- nc_open(choose.files()) #need to select "GLORYs112_salinity2" data

lon <- ncvar_get(dat, "longitude")
lat <- ncvar_get(dat, "latitude", verbose = F)
t <- ncvar_get(dat, "time")
depth <- ncvar_get(dat, "depth")

ndvi.array <- ncvar_get(dat) # store the data in a 3-dimensional array
dim(ndvi.array) 


# Create an empty list to store the raster objects
raster_list <- list()

for (i in 1:length(t)) {
  r <- raster(t(ndvi.array[, , i]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
  r <- flip(r, direction='y')
  # Append the current raster to the list
  raster_list[[i]] <- r
}
plot(raster_list[[336]])

#####
##### Dividing list of 336 months into 28 lists (years) of 12 (months) ####
# Initialize the number of sublists and elements per sublist
num_sublists <- 28
elements_per_sublist <- 12
# Initialize a list to store the separate sublists
separate_lists <- list()
# Split the raster_list into separate lists of elements_per_sublist each
for (i in seq(1, length(raster_list), by = elements_per_sublist)) {
  sublist <- raster_list[i:min(i + elements_per_sublist - 1, length(raster_list))]
  separate_lists <- append(separate_lists, list(sublist))
}
# Assign names to the separate sublists using a pattern
for (i in 1:num_sublists) {
  assign(paste0("sal", 1993 + i - 1), separate_lists[[i]])
}
# Check the created separate lists and their names
print(ls(pattern = "sal"))

##################################################

# Loop through years from 1993 to 2020
for (year in 1993:2020) {
  sublist_name <- paste0("sal", year)
  
  if (exists(sublist_name)) {  # Check if the sublist exists in the environment
    sublist <- get(sublist_name)  # Get the sublist by name
    
    mean_salinity <- Reduce(`+`, sublist) / length(sublist)
    assign(sublist_name, list(mean_salinity), envir = .GlobalEnv)  # Update the sublist in the environment
  }
}

plot(sal1993[[1]])

# Create an empty data frame to store the results
mean_salinity_df <- data.frame(Year = numeric(0), MeanSalinity = numeric(0))

# Loop through years from 1993 to 2020
for (year in 1993:2020) {
  sublist_name <- paste0("sal", year)
  
  if (exists(sublist_name)) {  # Check if the sublist exists in the environment
    raster_layer <- get(sublist_name)[[1]]  # Get the RasterLayer from the list
    
    mean_salinity <- mean(values(raster_layer), na.rm = TRUE)  # Calculate mean salinity
    mean_salinity_df <- mean_salinity_df %>%
      add_row(Year = year, MeanSalinity = mean_salinity)  # Add row to data frame
  }
}
# Print the resulting time series data frame
print(mean_salinity_df)

#write.csv(mean_salinity_df, "C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/Data/mean_salinity_GLORYs.csv", row.names=FALSE)
