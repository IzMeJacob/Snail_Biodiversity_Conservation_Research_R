library(readxl)
library(dplyr)
library(tidyr)

# 1. Set up environment
# Set working directory and define parameters
workpath <- "A:/BB/masters-thesis"
genus <- "Japonia"
species <- ""

setwd(workpath)

# Construct names
pred_plottitle <- paste(genus, species)
animal_path_name <- tolower(paste(genus, species, sep = "_"))
# excelData <- paste0(animal_path_name, ".xlsx")
excelData <- paste0(animal_path_name, ".csv")

# Construct paths
output_path <- paste0(workpath, "/data/rasters-outputs")
output_animal_path <- paste0(output_path, "/", animal_path_name)

# Construct directory
if(!file.exists(output_path)) dir.create(output_path)
if(!file.exists(output_animal_path)) dir.create(output_animal_path)

# Read the CSV data
df <- read.csv(paste0(workpath, "/data/", excelData))

df_summarized <- df %>%
  group_by(Species, Latitude, Longitude) %>%
  summarize(total_individuals = sum(Num.Individuals))

# Print the total sum
print(sum(df_summarized$total_individuals))

# Replicate rows based on the 'total_individuals' column
df_replicated <- df_summarized %>%
  uncount(weights = total_individuals)

# Write the replicated dataframe to a CSV file
replicated_dir <- paste0(workpath, "/data/replicated/")
if (!dir.exists(replicated_dir)) dir.create(replicated_dir)
write.csv(df_replicated, paste0(replicated_dir, animal_path_name, ".csv"), row.names = FALSE)
