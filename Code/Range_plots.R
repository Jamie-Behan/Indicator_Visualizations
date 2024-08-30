#### Create Distribution/ Range plots
library(here)
library(maps)
library(mapdata)
#### Load data ######
Lobster_range<-read.csv(here("Data/Range_data/Lobster_DisMAP.csv"))
Lobster_range <- Lobster_range[-c(1, 2), ]# Remove the first two rows
colnames(Lobster_range) <- Lobster_range[1, ]# Set the first row as the column names
Lobster_range <- Lobster_range[-1, ]# Remove the row that was used as the column names
rownames(Lobster_range) <- NULL# Reset the row numbers
head(Lobster_range)# View the updated dataframe
Lobster_range <- Lobster_range[, c("LAT", "LON", "Year", "wtcpue")]# Keep only the columns LAT, LON, Year, and wtcpue

Lobster_range <- Lobster_range %>% mutate(across(everything(), as.numeric)) #convert all columns to numeric

# Plot the distribution
png("Code/www/Lobster_range.png", width = 400, height = 400, res = 100)

# Set up the plot with a black background and other color modifications
par(mar = c(3.5, 4, 2, 2), mgp = c(2, 0.5, 0.1), bg = "#d0e6f3", col.lab = "black")  # Margins, axis positions, and black background

# Define the plot limits
lon_limits <- range(Lobster_range$LON) + c(-2.0, 2.0)  # Adjust longitude limits for better fit
lat_limits <- range(Lobster_range$LAT) + c(-2.0, 2.0)  # Adjust latitude limits for better fit

# Plot the map with a black background
plot(Lobster_range$LON, Lobster_range$LAT, pch = 16, col = "#b7c788", cex = 0.5,  # Light blue points
     xlim = c(lon_limits), ylim = c(lat_limits), 
     xlab = "Longitude", ylab = "Latitude", 
     axes = FALSE, bg = "#d0e6f3")

# Add the map with white borders and a dark gray fill
map(database = "worldHires", ylim = c(lat_limits), xlim = c(lon_limits), 
    col = "gray70", border = "gray30", fill = TRUE, add = TRUE)

# Add custom axes in white
axis(1, col = "black", col.axis = "black")  # White x-axis
axis(2, col = "black", col.axis = "black")  # White y-axis

# Add a title with white text
title(main = "American Lobster U.S. Range (1974-2023)", line = 0.5, cex.main = 1.0, col.main = "black")

# Add a legend in the bottom right corner
legend("bottomright", legend = "American Lobster U.S. Range", 
       pch = 16, col = "#b7c788", cex = 0.8, 
       text.col = "black", bg = "#d0e6f3", bty = "n")
legend("bottomleft", legend = "Data Sourced ", 
       pch = 16, col = "#b7c788", cex = 0.8, 
       text.col = "black", bg = "#d0e6f3", bty = "n")

# Close the device
dev.off()

##002b3e umaine blue
#ADD8E6 umaine light blue