library(raster)
library(sp)
library(dismo)
library(sf)
library(maps)
library(mapdata)
library(readxl)
library(dplyr)
library(rJava)

# Include "kuernm" library
# if(!require(devtools)){
#   install.packages("devtools")
# }
# 
# devtools::source_url("https://raw.githubusercontent.com/shandongfx/nimbios_enm/master/Appendix2_prepPara.R")
# 
# if(!require(kuenm)){
#   devtools::install_github("marlonecobos/kuenm")
# }
# library(kuenm)

# 1. Set up environment
# Set working directory and define parameters
workpath <- "A:/BB/masters-thesis"
genus <- "Japonia"
species <- "dido"
training_per <- 0.8

setwd(workpath)

# Construct names
pred_plottitle <- paste(genus, species)
pred_per_name <- ifelse(training_per == 0.5, "_50_50", "_80_20")
animal_path_name <- tolower(paste(genus, species, sep = "_"))
# excelData <- paste0(animal_path_name, ".xlsx")
excelData <- paste0(animal_path_name, ".csv")

# Create the expression with the variable substitution
# Current Suitability
predPlot_title <- bquote(atop("Predicted Current Suitability with MaxEnt", "("*italic(.(paste(genus, species)))*")"))
pred0omissionPlot_title <- bquote(atop("Predicted Current Suitability with MaxEnt (0% omission rate)", "("*italic(.(paste(genus, species)))*")"))

# Future Suitability
future_predPlot_title <- bquote(atop("Predicted Future Suitability with MaxEnt from 2021-2040", "("*italic(.(paste(genus, species)))*")"))
future_pred0omissionPlot_title <- bquote(atop("Predicted Future Suitability with MaxEnt from 2021-2040 (0% omission rate)", "("*italic(.(paste(genus, species)))*")"))

# Construct paths
output_path <- paste0(workpath, "/SDM/maxent_outputs/", animal_path_name)
current_output_path <- paste0(output_path, "/current")
future_output_path <- paste0(output_path, "/future")
clim_mask_path <- paste0(output_path, "/clim_mask")
prediction_path <- paste0(output_path, "/predictions")

# Construct predictions output paths
# Current Suitability
prediction_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction (MaxEnt).jpeg")
prediction_0omission_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction_0omission (MaxEnt).jpeg")
prediction_raster_path <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction_raster_MaxEnt.tif")

# Future Suitability
future_prediction_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_future_prediction (MaxEnt).jpeg")
future_prediction_0omission_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_future_prediction_0omission (MaxEnt).jpeg")
future_prediction_raster_path <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_future_prediction_raster_MaxEnt.tif")

# Construct directory
if(!file.exists("data/bioclim")) dir.create("data/bioclim")
if(!file.exists("data/bioclim/future")) dir.create("data/bioclim/future")
if(!file.exists(output_path)) dir.create(output_path)
if(!file.exists(current_output_path)) dir.create(current_output_path)
if(!file.exists(future_output_path)) dir.create(future_output_path)
if(!file.exists(clim_mask_path)) dir.create(clim_mask_path)
if(!file.exists(prediction_path)) dir.create(prediction_path)

# The spatial extent of Sarawak in Lon/Lat coordinates is roughly:
xmin <- 109  # Replace with your desired minimum longitude
xmax <- 116  # Replace with your desired maximum longitude
ymin <- 0.5 # Replace with your desired minimum latitude
ymax <- 5  # Replace with your desired maximum latitude
studyArea_ext <- extent(xmin, xmax, ymin, ymax)

# 2. Load Data

# Current bioclim
# Download current bioclimatic data
if (!file.exists("data/bioclim/wc2.1_2.5m_bio.zip")) {
  utils::download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip",
                       destfile = "data/bioclim/wc2.1_2.5m_bio.zip")
  utils::unzip("data/bioclim/wc2.1_2.5m_bio.zip", exdir = "data/bioclim")
}

# Read and stack bioclimatic data
clim_list <- list.files("data/bioclim", pattern = ".tif$", full.names = TRUE)

# stacking the bioclim variables to process them at one go
clim <- raster::stack(clim_list)

# Future bioclim
# Download future bioclimatic data
if (!file.exists("data/bioclim/future")) dir.create("data/bioclim/future")
if (!file.exists("data/bioclim/future/wc2.1_2.5m")) {
  cmip6_world("ACCESS-CM2", "585", "2021-2040", var="bioc", res=2.5, path="data/bioclim/future")
}

# List all .tif files in the directory
future_clim_files <- list.files("data/bioclim/future/wc2.1_2.5m", pattern = ".tif$", full.names = TRUE)

# Create a RasterBrick object from the TIFF file
# future_clim_stack_b <- brick(future_clim_files)
future_clim_stack <- stack(future_clim_files)

# Get the number of bands in the RasterBrick
num_bands <- nlayers(future_clim_stack)

# List the names of the bands (if not set, they may be automatically numbered)
band_names <- names(future_clim_stack)
print(band_names)

# Create a vector of new names (bio1, bio2, ..., bio19)
new_future_clim_names <- paste0("bio", 1:num_bands)

# Rename the bands in the RasterBrick
names(future_clim_stack) <- new_future_clim_names

# Verify the new names
print(names(future_clim_stack))

# Load map shape file
# map_shp <- shapefile(paste0(workpath, "data/Sarawak.shp"))

# Load your occurrence data (replace with your data structure)
# occ_raw <- read_excel(paste0(workpath, "/data/replicated/", excelData))
occ_raw <- read.csv(paste0(workpath, "/data/replicated/", excelData))

# Data Pre-Processing
occ_data <- occ_raw %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>% # Remove data-points with no complete coordinates
  distinct(Latitude, Longitude)  # Remove duplicates

cat(nrow(occ_data) - nrow(occ_raw), "records removed due to missing coordinates and duplicates.")

# Split data into training and testing sets
training_indices <- sample(1:nrow(occ_data), nrow(occ_data) * training_per)
occ_train <- occ_data[training_indices, ]
occ_test <- occ_data[-training_indices, ]

# Create copies of occ_data, training and testing sets
occ_data_sp <- occ_data
occ_train_sp <- occ_train
occ_test_sp <- occ_test

# Make occ_data spatial
coordinates(occ_data_sp) <- ~Longitude + Latitude
coordinates(occ_train_sp) <- ~Longitude + Latitude
coordinates(occ_test_sp) <- ~Longitude + Latitude

# Define Coordinate Reference System (CRS)
occ_data_CRS <- sf::st_as_sf(occ_data_sp, coords = c("Longitude", "Latitude"), crs = 4326)
occ_train_CRS <- sf::st_as_sf(occ_train_sp, coords = c("Longitude", "Latitude"), crs = 4326)
occ_test_CRS <- sf::st_as_sf(occ_test_sp, coords = c("Longitude", "Latitude"), crs = 4326)

# Create Buffer
# Using sf package for cleaner syntax and potentially better performance:
occ_buffer <- sf::st_buffer(occ_data_CRS, dist = 400000)  # Buffer in meters
# occ_buffer <- sf::st_buffer(occ_data_CRS, dist = 0.005)  # Buffer in meters

# Mask Climate Data
# Clip raster using shapefile boundary
# clim_crop <- crop(clim, map_shp)

# Current bioclim
clim_crop <- raster::crop(clim, studyArea_ext)
clim_mask <- raster::mask(clim_crop, occ_buffer)

plot(clim_mask[[1]])
plot(occ_buffer, add = T, col = "blue")

# Future bioclim
future_clim_crop <- raster::crop(future_clim_stack, studyArea_ext)
future_clim_mask <- raster::mask(future_clim_crop, occ_buffer)

plot(future_clim_mask[[1]])
plot(occ_buffer, add = T, col = "blue")

# Select specific climate layers
# Choose relevant variables based on your knowledge and pre-processing (e.g., VIF analysis)
# Adjust based on your selection
selected_vars <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6",
                   "bio7", "bio8", "bio9", "bio10", "bio11", "bio12",
                   "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"
)

clim_mask_selected <- clim_mask[[selected_vars]]
future_clim_mask_selected <- future_clim_mask[[selected_vars]]

# set plot layout
par(mfrow = c(5, 4), mar = c(1, 1, 1, 1))

# Select specific climate layer
# Current bioclim
for (i in 1:nlayers(clim_mask_selected)) {
  main = paste0("bio", i, " (Current bioclim)")
  plot(clim_mask_selected[[i]], main=main)
  plot(occ_data_CRS, add = TRUE, col = "red") # plot the occ_data on the above raster layer
  
  # Create the map with extent
  map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)
}

# Future bioclim
for (i in 1:nlayers(future_clim_mask_selected)) {
  main = paste0("bio", i, " (Future bioclim)")
  plot(future_clim_mask_selected[[i]], main=main)
  plot(occ_data_CRS, add = TRUE, col = "red") # plot the occ_data on the above raster layer
  
  # Create the map with extent
  map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)
}

# set plot layout
par(mfrow = c(1, 1))

# ----- Prepare data for Maxent

# --- Current bioclim
# Generate background points
set.seed(1)
bg <- sampleRandom(x = clim_mask_selected,
                   size = 10000,
                   na.rm = TRUE,  # Remove "Not Applicable" points
                   sp = TRUE)  # Return spatial points

plot(clim_mask_selected[[1]])
# add the background points to the plotted raster
plot(bg,add=T)
# add the occurrence data to the plotted raster
plot(occ_data_CRS,add=T,col="red")

# Generate environmental conditions
env_occ <- extract(clim_mask_selected, occ_data_CRS)
env_occ_train <- extract(clim_mask_selected, occ_train_CRS)
env_occ_test <- extract(clim_mask_selected, occ_test_CRS)
env_bg <- extract(clim_mask_selected, bg)

# Generate presence-absence
p <- c(rep(1, nrow(env_occ)))
pa_full <- c(rep(1, nrow(env_occ)), rep(0, nrow(env_bg)))
pder_full <- rbind(env_occ, env_bg)
pa_train <- c(rep(1, nrow(env_occ_train)), rep(0, nrow(env_bg)))
pder_train <- rbind(env_occ_train, env_bg)

# Process presence-absence data (merge presence-absence with environment)
# model_data = cbind(pa_full, pder_full) # presence-absence and presence-absence env (full)
# model_data2 = cbind(p, env_occ) # presence and presence env (presence only)

# Convert data for model into dataframe
# pa_full_df <- data.frame(pa_full) # only presence-absence (full)
# pder_full_df <- data.frame(pder_full) # only presence-absence env (full)
pder_train_df <- as.data.frame(pder_train) # presence-absence and presence-absence env (training)
# model_data_df <- data.frame(model_data) # presence-absence and presence-absence env (full)

# --- Future bioclim
# Generate background points
set.seed(1)
future_bg <- sampleRandom(x = future_clim_mask_selected,
                   size = 10000,
                   na.rm = TRUE,  # Remove "Not Applicable" points
                   sp = TRUE)  # Return spatial points

plot(future_clim_mask_selected[[1]])
# add the background points to the plotted raster
plot(future_bg,add=T)
# add the occurrence data to the plotted raster
plot(occ_data_CRS,add=T,col="red")

# Generate environmental conditions
future_env_occ <- extract(future_clim_mask_selected, occ_data_CRS)
future_env_occ_train <- extract(future_clim_mask_selected, occ_train_CRS)
future_env_occ_test <- extract(future_clim_mask_selected, occ_test_CRS)
future_env_bg <- extract(future_clim_mask_selected, future_bg)

# Generate presence-absence
future_p <- c(rep(1, nrow(future_env_occ)))
future_pa_full <- c(rep(1, nrow(future_env_occ)), rep(0, nrow(future_env_bg)))
future_pder_full <- rbind(future_env_occ, future_env_bg)
future_pa_train <- c(rep(1, nrow(future_env_occ_train)), rep(0, nrow(future_env_bg)))
future_pder_train <- rbind(future_env_occ_train, future_env_bg)

# Process presence-absence data (merge presence-absence with environment)
# future_model_data = cbind(future_pa_full, future_pder_full) # presence-absence and presence-absence env (full)
# future_model_data2 = cbind(future_p, future_env_occ) # presence and presence env (presence only)

# Convert data for model into dataframe
# future_pa_full_df <- data.frame(future_pa_full) # only presence-absence (full)
# future_pder_full_df <- data.frame(future_pder_full) # only presence-absence env (full)
future_pder_train_df <- as.data.frame(future_pder_train) # presence-absence and presence-absence env (training)
# future_model_data_df <- data.frame(future_model_data) # presence-absence and presence-absence env (full)

# Define model formula
# model_formula <- pa_raw ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19 # Adjust variable names

# ----- Run Maxent modeling

# --- Current bioclim
# Consider exploring regularization and background sampling options
maxent_mod <- maxent(x=pder_train_df, ## env conditions
                     p=pa_train,   ## 1:presence or 0:absence
                     path=paste0(current_output_path), ## folder for maxent output; 
                     # if we do not specify a folder R will put the results in a temp file, 
                     # and it gets messy to read those. . .
                     args=c("responsecurves") ## parameter specification
                    )

# view the maxent model in a html brower
maxent_mod

# view detailed results
maxent_mod@results

# example 1, project to study area
# Export jpeg
jpeg(prediction_jpeg, unit= "px", width=800, height=500)
ped1 <- predict(maxent_mod, clim_mask)  # studyArea is the clipped rasters 
plot(ped1, main=predPlot_title) # plot the continuous prediction
plot(occ_data_CRS,add=T,col="red") # add the occurrence data to the plotted raster
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

dev.off()

# Evaluate training occurrence data
mod_eval_train <- dismo::evaluate(p = env_occ_train, a = env_bg, model = maxent_mod)
print(mod_eval_train)

# Evaluate test occurrence data
mod_eval_test <- dismo::evaluate(p = env_occ_test, a = env_bg, model = maxent_mod)
print(mod_eval_test)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
# Export jpeg
jpeg(prediction_0omission_jpeg, unit= "px", width=800, height=500)
plot(ped1 >= thd1, main=pred0omissionPlot_title)
plot(occ_data_CRS,add=T,col="red") # add the occurrence data to the plotted raster
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

dev.off()

# Define the target resolution
resolution <- 0.002694946 

# Create a template raster with the desired resolution and extent
template_raster <- raster(extent(ped1), res = resolution)

# Resample the predicted raster to the desired resolution
ped1_resampled <- resample(ped1, template_raster, method = "bilinear")

# Plot the resampled raster to check
plot(ped1_resampled, main = "Resampled Predicted Current Suitability")
plot(occ_data_CRS, add = T, col = "red")
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

# Save the resampled raster
prediction_raster_resampled_path <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction_raster_resampled_MaxEnt.tif")
writeRaster(ped1_resampled, filename = prediction_raster_resampled_path, format = "GTiff", overwrite = TRUE)

# --- Future bioclim
# Consider exploring regularization and background sampling options
future_maxent_mod <- maxent(x=future_pder_train_df, ## env conditions
                     p=future_pa_train,   ## 1:presence or 0:absence
                     path=paste0(future_output_path), ## folder for maxent output; 
                     # if we do not specify a folder R will put the results in a temp file, 
                     # and it gets messy to read those. . .
                     args=c("responsecurves") ## parameter specification
)

# view the maxent model in a html brower
future_maxent_mod

# view detailed results
future_maxent_mod@results

# example 1, project to study area
# Export jpeg
jpeg(future_prediction_jpeg, unit= "px", width=800, height=500)
future_ped1 <- predict(future_maxent_mod, future_clim_mask)  # studyArea is the clipped rasters 
plot(future_ped1, main=future_predPlot_title) # plot the continuous prediction
plot(occ_data_CRS,add=T,col="red") # add the occurrence data to the plotted raster
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

dev.off()

# Evaluate training occurrence data
future_mod_eval_train <- dismo::evaluate(p = future_env_occ_train, a = future_env_bg, model = future_maxent_mod)
print(future_mod_eval_train)

# Evaluate test occurrence data
future_mod_eval_test <- dismo::evaluate(p = future_env_occ_test, a = future_env_bg, model = future_maxent_mod)
print(future_mod_eval_test)  # training AUC may be higher than testing AUC

# calculate thresholds of models
future_thd1 <- threshold(future_mod_eval_train, "no_omission")  # 0% omission rate 
future_thd2 <- threshold(future_mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
# Export jpeg
jpeg(future_prediction_0omission_jpeg, unit= "px", width=800, height=500)
plot(future_ped1 >= future_thd1, main=future_pred0omissionPlot_title)
plot(occ_data_CRS,add=T,col="red") # add the occurrence data to the plotted raster
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

dev.off()

# Define the target resolution
resolution <- 0.002694946 

# Create a template raster with the desired resolution and extent
future_template_raster <- raster(extent(future_ped1), res = resolution)

# Resample the predicted raster to the desired resolution
future_ped1_resampled <- resample(future_ped1, future_template_raster, method = "bilinear")

# Plot the resampled raster to check
plot(future_ped1_resampled, main = "Resampled Predicted Future Suitability")
plot(occ_data_CRS, add = T, col = "red")
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

# Save the resampled raster
future_prediction_raster_resampled_path <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_future_prediction_raster_resampled_MaxEnt.tif")
writeRaster(future_ped1_resampled, filename = future_prediction_raster_resampled_path, format = "GTiff", overwrite = TRUE)