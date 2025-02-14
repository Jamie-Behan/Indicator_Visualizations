library(dplyr)
library(terra)
library(utils)
####### Load in dismap .csv files from dismap folder 
# Set the directory path where your .csv files are located

#csv_files <- list.files("C:/Users/crazi/Box/Kerr Lab/Fisheries Science Lab/NCLIM/Indicator_Visualizations/Indicator_Visualizations2/Deploy_Shiny", pattern = "distribution-metrics-data.*\\.csv$", full.names = TRUE) # Get a list of all .csv files in the folder
csv_files <- list.files("./", pattern = "distribution-metrics-data.*\\.csv$", full.names = TRUE)
data_frames <- list() # Initialize an empty list to store the data frames

# Loop through the list of .csv files and read each file as a data frame
for (file in csv_files) { 
  df_name <- tools::file_path_sans_ext(basename(file)) # Extract the file name without extension to use as the data frame name
  data_frames[[df_name]] <- read.csv(file) # Read the .csv file and store it as a data frame in the list
}
# View each data frame in the list
#for (df_name in names(data_frames)) {
#  View(data_frames[[df_name]])
#}
######## Rename each dataframe in the list based on species and season
# Loop through each data frame in the list
for (df_name in names(data_frames)) {
  # Get the column name of the first column
  first_column_name <- names(data_frames[[df_name]])[1]
  
  # Check if "Spring" or "Fall" is present in the column name of the first column
  if (grepl("Spring", first_column_name)) {
    season <- "Spring"
  } else if (grepl("Fall", first_column_name)) {
    season <- "Fall"
  } else {
    stop("Neither 'Spring' nor 'Fall' found in the column name of the first column of ", df_name)
  }
  
  # Extract the first row and get the first two words after "Species Name:"
  first_row <- data_frames[[df_name]][1, 1]  # Assuming the first column is the first column in the dataframe
  species_words <- unlist(strsplit(first_row, " "))  # Split the first row by spaces
  
  # If there are more than two words after "Species Name:", take the first two words
  if (length(species_words) >= 3) {
    species_name <- paste(species_words[3], species_words[4], sep = "_")
  } else if (length(species_words) == 2) {
    species_name <- paste(species_words[1], species_words[2], sep = "_")
  } else {
    stop("Could not extract species name from the first row of ", df_name)
  }
  
  # Generate the new name for the data frame
  new_name <- paste(species_name, season, sep = "_")
  
  # Rename the data frame
  names(data_frames)[names(data_frames) == df_name] <- new_name
}
# Print the names of the renamed data frames
print(names(data_frames))

#View(data_frames[["American_lobster_Spring"]])
########### deleting first two rows and making row 3 the new column names 
# Loop through each data frame in the list
for (df_name in names(data_frames)) {
  # Delete the first two rows of the data frame
  data_frames[[df_name]] <- data_frames[[df_name]][-c(1, 2), , drop = FALSE]
  
  # Set the text in each column of the new first row as column names
  new_colnames <- as.character(data_frames[[df_name]][1, ])
  colnames(data_frames[[df_name]]) <- new_colnames
  
  # Remove the first row as it's now used as column names
  data_frames[[df_name]] <- data_frames[[df_name]][-1, , drop = FALSE]
}

#View(data_frames[["American_lobster_Spring"]])

######### Remove "Min Lat" and "Max Lat" columns from each data frame in the list
data_frames <- lapply(data_frames, function(df) {
  # Use setdiff() to remove specified columns
  cols_to_remove <- c("Min Lat", "Max Lat")
  df <- df[setdiff(names(df), cols_to_remove)]
  return(df)
})

#View(data_frames[["American_lobster_Spring"]])
########### Merging DFs and renaming columns 
# Function to rename columns based on species name and season
rename_columns <- function(df, species, season) {
  colnames(df) <- gsub("COG Lat", paste(species, season, "Lat_COG", sep = "_"), colnames(df))
  colnames(df) <- gsub("COG depth", paste(species, season, "Depth_COG", sep = "_"), colnames(df))
  return(df)
}

# Rename columns for each data frame in the list
for (df_name in names(data_frames)) {
  data_frames[[df_name]] <- rename_columns(data_frames[[df_name]], df_name, "Season")  # Replace "Season" with the actual season name
}


# Merge all data frames by the "Year" column
merged_df <- Reduce(function(x, y) merge(x, y, by = "YEAR", all = TRUE), data_frames)
# Get column names containing the word "Season"
season_columns <- grepl("Season", names(merged_df))
# Modify column names by removing "Season"
names(merged_df)[season_columns] <- gsub("_Season", "", names(merged_df)[season_columns])
# Rename "YEAR" column to "Year" because the other dfs used in the app use "Year"
colnames(merged_df)[colnames(merged_df) == "YEAR"] <- "Year"
DisMAPdata<-merged_df
rm(merged_df,data_frames)
