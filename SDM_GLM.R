library(raster)
library(sp)
library(dismo)
library(sf)
library(maps)
library(mapdata)
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(pROC)  # for ROC analysis

# Include "mecofun" library
if(!require(devtools)){
  install.packages("devtools")
}
if(!require(mecofun)){
  devtools::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")
}
library(mecofun)

# 1. Set up environment
# Set working directory and define parameters
workpath <- "A:/BB/SDM"
genus <- "Japonia"
species <- "barbata"
training_per <- 0.8

setwd(workpath)

# Construct names
pred_plottitle <- paste(genus, species)
pred_per_name <- ifelse(training_per == 0.5, "_50_50", "_80_20")
animal_path_name <- tolower(paste(genus, species, sep = "_"))
excelData <- paste0(animal_path_name, ".xlsx")

# Create the expression with the variable substitution
predPlot_title <- bquote(atop("Predicted Current Suitability with GLM", "("*italic(.(paste(genus, species)))*")"))
pred0omissionPlot_title <- bquote(atop("Predicted Current Suitability with GLM (0% omission rate)", "("*italic(.(paste(genus, species)))*")"))

# Construct paths
output_path <- paste0(workpath, "/GLM_outputs/", animal_path_name)
clim_mask_path <- paste0(output_path, "/clim_mask")
prediction_path <- paste0(output_path, "/predictions")

# Construct predictions output paths
prediction_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction (GLM).jpeg")
prediction_0omission_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_prediction_0omission (GLM).jpeg")
prediction_raster_path <- paste0(prediction_path, "/", animal_path_name, "_prediction_raster (GLM).tif")
ROC_jpeg <- paste0(prediction_path, "/", animal_path_name, pred_per_name, "_ROC (GLM).jpeg")

# Construct directory
if(!file.exists("GLM_outputs")) dir.create("GLM_outputs")
if(!file.exists("data/bioclim")) dir.create("data/bioclim")
if(!file.exists(output_path)) dir.create(output_path)
if(!file.exists(clim_mask_path)) dir.create(clim_mask_path)
if(!file.exists(prediction_path)) dir.create(prediction_path)

# The spatial extent of Sarawak in Lon/Lat coordinates is roughly:
xmin <- 108  # Replace with your desired minimum longitude
xmax <- 116  # Replace with your desired maximum longitude
ymin <- 0 # Replace with your desired minimum latitude
ymax <- 5  # Replace with your desired maximum latitude
studyArea_ext <- extent(xmin, xmax, ymin, ymax)

# 2. Load Data
# Download bioclimatic data (replace with relevant URLs)
if (!file.exists("data/bioclim/wc2.1_2.5m_bio.zip")) {
  utils::download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip",
                       destfile = "data/bioclim/wc2.1_2.5m_bio.zip")
  utils::unzip("data/bioclim/wc2.1_2.5m_bio.zip", exdir = "data/bioclim")
}

# Read and stack bioclimatic data
clim_list <- list.files("data/bioclim", pattern = ".tif$", full.names = TRUE)

# stacking the bioclim variables to process them at one go
clim <- raster::stack(clim_list)

# Load map shape file
# map_shp <- shapefile(paste0(workpath, "data/Sarawak.shp"))

# Load your occurrence data (replace with your data structure)
occ_raw <- read_excel(paste0(workpath, "/data/", excelData))

# 3. Data Pre-Processing
occ_data <- occ_raw %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>% # Remove data-points with no complete coordinates
  distinct(Latitude, Longitude)  # Remove duplicates

cat(nrow(occ_data) - nrow(occ_raw), "records removed due to missing coordinates")

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

# Mask Climate Data
# Clip raster using shapefile boundary
# clim_crop <- crop(clim, map_shp)
clim_crop <- raster::crop(clim, studyArea_ext)
clim_mask <- raster::mask(clim_crop, occ_buffer)

# Select specific climate layers
# Choose relevant variables based on your knowledge and pre-processing (e.g., VIF analysis)
# Adjust based on your selection
selected_vars <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6",
                   "bio7", "bio8", "bio9", "bio10", "bio11", "bio12",
                   "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"
)
clim_mask_selected <- clim_mask[[selected_vars]]

# set plot layout
par(mfrow = c(5, 4), mar = c(1, 1, 1, 1))

# Select specific climate layer
for (i in 1:nlayers(clim_mask_selected)) {
  main = paste0("bio", i, " (bioclimate mask)")
  plot(clim_mask_selected[[i]], main=main)
  plot(occ_data_CRS, add = TRUE, col = "red") # plot the occ_data on the above raster layer
  
  # Create the map with extent
  map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)
}

# set plot layout
par(mfrow = c(1, 1))

# Prepare data for Maxent
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
model_data = cbind(pa_full, pder_full) # presence-absence and presence-absence env (full)
model_data2 = cbind(p, env_occ) # presence and presence env (presence only)

# Convert presence_absence data into dataframe
# pa_raw_df <- data.frame(pa_raw) # only presence-absence (full)
# pder_raw_df <- data.frame(pder_raw) # only presence-absence env (full)
model_data_df <- data.frame(model_data) # presence-absence and presence-absence env (full)

# Define model formula
model_formula <- pa_full ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19 # Adjust variable names

# Species Distribution Modeling using GLM
glm_mod <- glm(formula = model_formula, family='binomial', data=model_data_df)

# We want three panels next to each other:
par(mfrow=c(1,3))

# Plot the partial responses
partial_response(glm_mod, predictors = model_data_df[,selected_vars])

# Make predictions to current climate:
clim_mask_df <- data.frame(rasterToPoints(clim_mask))

# Predict probability from GLM model
clim_mask_df$glm_pred <- mecofun::predictSDM(glm_mod, clim_mask_df)

# We want three panels next to each other:
par(mfrow=c(1,1))

mod_eval_train <- dismo::evaluate(p = env_occ, a = env_bg, model = glm_mod)
print(mod_eval_train)

# Make raster stack of predictions:
r_pred_curr <- rasterFromXYZ(clim_mask_df[,-c(3:21)])

jpeg(prediction_jpeg, unit= "px", width=800, height=800)
plot(r_pred_curr)
plot(occ_data_CRS, add = TRUE, col = "red") # plot the occ_data on the above raster layer
# Create the map with extent
map(region = NULL, xlim = c(xmin, xmax), ylim = c(ymin, ymax), fill = FALSE, add = TRUE)

jpeg(ROC_jpeg, unit= "px", width=800, height=800)
pred_probs <- predict(glm_mod, type = "response")
roc_object <- roc(model_data_df$pa_full ~ pred_probs, data = model_data_df)
# Plot the ROC curve with desired customizations
plot(roc_object, print.auc = TRUE, col = "red", 
     # Set color and add grid lines with lwd = 0.2 for lighter lines
     pch = 19,  # Point character for markers (optional)
     lwd = 0.2,  # Line width
     grid = TRUE)

# Adjust axis labels using xlab and ylab
xlab("False Positive Rate (FPR)")
ylab("True Positive Rate (TPR)")
title(main = "ROC Curve (GLM)")

dev.off()
