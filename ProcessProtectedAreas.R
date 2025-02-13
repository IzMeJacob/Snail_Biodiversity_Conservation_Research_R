# Install necessary packages if not already installed
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")

# Load the libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Download Malaysia boundary
malaysia_boundary <- ne_countries(scale = "medium", country = "Malaysia", returnclass = "sf")

# Define the bounding box for Malaysia
malaysia_bbox <- st_bbox(malaysia_boundary)

# Function to read and crop a shapefile using a bounding box
read_and_crop <- function(shape, bbox) {
  cropped_shape <- st_crop(shape, bbox)
  return(cropped_shape)
}

# Function to intersect a shapefile with a boundary
intersect_with_boundary <- function(shapefile, boundary) {
  intersection <- st_intersection(shapefile, boundary)
  return(intersection)
}

# Set the path to your shapefiles
PA_shp_0 <- "A:/BB/PA/shp/shp_0/WDPA_Jun2024_Public_shp-polygons.shp"
PA_shp_1 <- "A:/BB/PA/shp/shp_1/WDPA_Jun2024_Public_shp-points.shp"
PA_shp_2 <- "A:/BB/PA/shp/shp_2/WDPA_Jun2024_Public_shp-points.shp"

protected_area_shape_0 <- st_read(PA_shp_0)
# protected_area_shape_0 <- read_and_crop(protected_area_shape_0, malaysia_bbox)
protected_area_shape_0 <- intersect_with_boundary(protected_area_shape_0, malaysia_boundary)

# protected_area_shape_1 <- st_read(PA_shp_1)
# protected_area_shape_1 <- read_and_crop(protected_area_shape_1, malaysia_boundary)
# 
# protected_area_shape_2 <- st_read(PA_shp_2)
# protected_area_shape_2 <- read_and_crop(protected_area_shape_2, malaysia_boundary)

# Print a summary of the shapefile
print(protected_area_shape_0)

# Basic plot using base R
plot(st_geometry(protected_area_shape_0), main="Protected Areas In Borneo")

# Combine the cropped shapefiles into a single sf object
# protected_area_shape <- rbind(protected_area_shape_0, protected_area_shape_1, protected_area_shape_2)

# Advanced plot using ggplot2
# Download a world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot using ggplot2 with a map background
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # World basemap
  geom_sf(data = protected_area_shape_0, fill = "lightblue", color = "darkblue", alpha = 0.5) +
  theme_minimal() +
  coord_sf(xlim = c(108, 120), ylim = c(0, 8)) +  # Adjust to focus on Malaysia region
  ggtitle("Protected Areas in Borneo")
