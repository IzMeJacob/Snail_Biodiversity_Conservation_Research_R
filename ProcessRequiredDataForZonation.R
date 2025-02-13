library(raster)
library(sp)
library(sf)
library(terra)
library(maps)
library(mapdata)
library(readxl)

# Set working directory and define parameters
workpath <- "A:/BB/masters-thesis"
genus <- "Japonia"
species <- "sp Niah"
training_per <- 0.8

setwd(workpath)

# Construct names
pred_plottitle <- paste(genus, species)
animal_path_name <- tolower(paste(genus, species, sep = "_"))
# excelData <- paste0(animal_path_name, ".xlsx")
excelData <- paste0(animal_path_name, ".csv")

# Construct paths
output_path <- paste0(workpath, "/data/rasters-outputs")
# output_animal_path <- paste0(output_path, "/", animal_path_name)

# Construct directory
if(!file.exists(output_path)) dir.create(output_path)
# if(!file.exists(output_animal_path)) dir.create(output_animal_path)

# The spatial extent of Sarawak in Lon/Lat coordinates is roughly:
xmin <- 108  # Replace with your desired minimum longitude
xmax <- 120  # Replace with your desired maximum longitude
ymin <- 0 # Replace with your desired minimum latitude
ymax <- 7.5  # Replace with your desired maximum latitude
studyArea_ext <- extent(xmin, xmax, ymin, ymax)

# Define cell size
res <- 0.002694946 # Adjust based on your needs

# Create raster template
r <- raster(studyArea_ext, resolution = res)
r[] <- NA

# Sarawak shp
# Load map shape file
state_shape <- st_read(paste0(workpath, "/data/Sarawak.shp"))

plot(state_shape, max.plot = 13)

# Rasterize the shapefile, setting cells within the state to 0
# state_raster <- rasterize(as(state_shape, "Spatial"), r, field = 1, background = NA)
# state_raster[state_raster == 1] <- 0

## Set up a raster "template" to use in rasterize()
xy <- abs(apply(as.matrix(bbox(studyArea_ext)), 1, diff))
n <- 5
raster_template <- raster(studyArea_ext, ncol=xy[1]*n, nrow=xy[2]*n, resolution = res)
# raster_template <- raster(studyArea_ext, resolution = res)
# raster_template[] <- NA

# Function to map category to raster values
category_to_value <- function(category) {
  switch(category,
         "Strict nature reserve" = 7,
         "Wilderness Area" = 6,
         "National park" = 5,
         "Natural monument or Feature" = 4,
         "Habitat/Species Management Area" = 3,
         "Protected Landscape/Seascape" = 2,
         "Protected Area with sustainable use of natural resources" = 1,
         NA)
}

# Define category to numeric mapping
# category_to_numeric <- c(
#   # "Strict nature reserve" = 7,
#   "National Reserve" = 7,
#   # "Wilderness Area" = 6,
#   "Wildlife Sanctuary" = 6,
#   "National Park" = 5,
#   "Natural monument or Feature" = 4,
#   "Habitat/Species Management Area" = 3,
#   "Protected Landscape/Seascape" = 2,
#   "Protected Area with sustainable use of natural resources" = 1
# )
category_to_numeric <- c(
  "National Reserve" = 3,
  "Wildlife Sanctuary" = 2,
  "National Park" = 1
)

# Sarawak PA shp
# Load map shape file
state_pa_shape <- shapefile(paste0(workpath, "/data/PA/Sarawak_protected_areas.shp"))
state_pa_shape_stdf <- st_read(paste0(workpath, "/data/PA/Sarawak_protected_areas.shp"))

# Check unique categories in the "category" field
unique_categories <- unique(state_pa_shape$category)

# Print the unique categories
print(unique_categories)

# Create PA index
state_pa_shape$category_index = category_to_numeric[state_pa_shape$category]

# Plot the state_pa_shape object with all attributes
# plot(state_pa_shape, max.plot = 10)

## Rasterize the shapefile
# Rasterize shapefile onto raster_template
state_pa_raster1 <- rasterize(state_pa_shape, raster_template)
# state_pa_raster2 <- rasterize(state_pa_shape, raster_template, field = "category", fun = category_to_value)
state_pa_raster2 <- rasterize(state_pa_shape, raster_template, field = "category_index")
plot(state_pa_raster1, main = "state_pa_raster (elvation)")
plot(state_pa_raster2, main = "state_pa_raster (Protected Area Category Index)")

# Load your occurrence data (replace with your data structure)
# occ_raw <- read_excel(paste0(workpath, "/data/", excelData))
occ_raw <- read.csv(paste0(workpath, "/data/replicated/", excelData))

occ_data <- occ_raw 

# Data Pre-Processing
# occ_data <- occ_raw %>%
#   filter(!is.na(Latitude) & !is.na(Longitude)) %>% # Remove data-points with no complete coordinates
#   distinct(Latitude, Longitude)  # Remove duplicates
# 
# cat(nrow(occ_data) - nrow(occ_raw), "records removed due to missing coordinates and duplicates.")

# Turn coords to spdf
occ_data_spdf <- sp::SpatialPointsDataFrame(coords = cbind(occ_data$Longitude, occ_data$Latitude),
                                            data = occ_data,
                                            proj4string = CRS("+proj=longlat +datum=WGS84"))

# Rasterize the distribution points, setting cells to 1

# Rasterise distribution points
# occ_data_raster <- rasterize(occ_data_spdf, r, field = 1, fun = "count", background = 0)
# occ_data_rasterized = rasterize(occ_data_spdf@coords, occ_data_raster, name="data", fun=mean)

# occ_data_raster <- raster(studyArea_ext, crs=occ_data_spdf@proj4string, vals=1,
#                         xmn=min(studyArea_ext), xmx=max(studyArea_ext),
#                         ymn=min(studyArea_ext), ymx=max(studyArea_ext),
#                         resolution=res)

# Rasterize the distribution points, setting cells to 1 for presence
# occ_data_raster <- rasterize(occ_data_spdf, r, field = 1, fun = function(x, ...) { 1 }, background = NA)

# Rasterize the distribution points, setting cells to 1 for presence without stacking
# Create a unique identifier for each occurrence point
occ_data_spdf$ID <- 1:nrow(occ_data_spdf)

# Rasterize the distribution points, setting cells to the unique identifier
occ_data_raster <- rasterize(occ_data_spdf, r, field = "ID", fun = "count", background = NA)

# Combine the state raster and the distribution raster
combined_raster <- cover(occ_data_raster, state_raster)

# Visualize the combined raster
plot(combined_raster, main = "Distribution Points (1) and State (0)")

# Human footprint raster pre-process
footprint_raster <- raster("A:/BB/masters-thesis/data/rasters-outputs/hii_2020-01-01.tif")

# Resample footprint to match spdf extent and resolution
footprint_cropped <- raster::crop(footprint_raster, studyArea_ext)

# plot(footprint_cropped)

# Get the resolution of the cropped raster
footprint_res <- res(footprint_cropped)

# Calculate the product of the resolution to obtain the area of each cell
cell_area <- prod(footprint_res)

# Print the resolution and cell area
cat("Human Footprint Resolution (x, y):", footprint_res[1], footprint_res[2], "\n")
cat("Cell area:", cell_area, "square map units\n")

# Resample the rasterized raster to match the resolution of footprint_cropped
# occ_data_rasterized_resampled <- resample(occ_data_rasterized, footprint_croppedocc_data_spdf, method = "bilinear")

# Print the resolution of the resampled raster
# cat("Resolution of occ_data_rasterized_resampled (x, y):", res(occ_data_rasterized_resampled)[1], res(occ_data_rasterized_resampled)[2], "\n")

# footprint_resampled <- raster::resample(footprint_cropped, resolution=res)

library(leaflet)

# Create a leaflet map
map <- leaflet() %>%
  addTiles()  # Add a base map

# Add the raster layer to the map
map <- addRasterImage(map, state_pa_raster2)

# Display map
map

# Export species distribution raster
# writeRaster(combined_raster, paste0(output_path, "/", animal_path_name, ".tif"), format = "GTiff", overwrite=TRUE)

# Export rasters
writeRaster(state_pa_raster2, paste0(output_path, "/sarawak_protected_areas_category_index.tif"), format = "GTiff", overwrite=TRUE, , datatype='INT1U')

# Write the downscaled footprint raster
# writeRaster(footprint_cropped, paste0(output_path, "/footprint_cropped.tif"), format = "GTiff", overwrite=TRUE)
