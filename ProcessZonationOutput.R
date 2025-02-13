library(raster)
library(sf)
library(RColorBrewer)

# Example of a custom color palette
colors <- colorRampPalette(c("blue", "yellow", "red"))(100)
cuts=c(0,20,50,75,90,95,98,100) #set breaks
pal <- colorRampPalette(c("black","darkblue", "blue", "yellow", "magenta", "brown", "red"))

# Define the spatial extent of your study area
xmin <- 108   # Minimum longitude
xmax <- 120   # Maximum longitude
ymin <- 0     # Minimum latitude
ymax <- 7.5   # Maximum latitude
studyArea_ext <- extent(xmin, xmax, ymin, ymax)

# Define the spatial extent of your study area
xmin <- 109   # Minimum longitude
xmax <- 116   # Maximum longitude
ymin <- 0.5     # Minimum latitude
ymax <- 5   # Maximum latitude
new_studyArea_ext <- extent(xmin, xmax, ymin, ymax)

# Define the target CRS
target_crs <- CRS("+proj=longlat +datum=WGS84")  # Replace with your desired CRS

# Load the PNG image
zonation_uncon_r <- raster("A:/BB/masters-thesis/Zonation/zonation_uncon.png")
zonation_PA_r <- raster("A:/BB/masters-thesis/Zonation/zonation_PA.png")
zonation_PA_humanfoot_r <- raster("A:/BB/masters-thesis/Zonation/zonation_PA_humanfoot.png")

# Convert 0 values to NA
zonation_uncon_r[zonation_uncon_r == 0] <- NA
zonation_PA_r[zonation_PA_r == 0] <- NA
zonation_PA_humanfoot_r[zonation_PA_humanfoot_r == 0] <- NA

# Set the extent of the raster
extent(zonation_uncon_r) <- studyArea_ext
extent(zonation_PA_r) <- studyArea_ext
extent(zonation_PA_humanfoot_r) <- studyArea_ext

# Set the CRS of the raster
projection(zonation_uncon_r) <- target_crs
projection(zonation_PA_r) <- target_crs
projection(zonation_PA_humanfoot_r) <- target_crs

zonation_uncon_r_crop <- crop(zonation_uncon_r, new_studyArea_ext)
zonation_PA_r_crop <- crop(zonation_PA_r, new_studyArea_ext)
zonation_PA_humanfoot_r_crop <- crop(zonation_PA_humanfoot_r, new_studyArea_ext)

# Check the minimum and maximum values
min_value <- minValue(zonation_uncon_r_crop)
max_value <- maxValue(zonation_uncon_r_crop)

cat("Minimum value:", min_value, "\n")
cat("Maximum value:", max_value, "\n")
summary(zonation_uncon_r_crop)

# Define the new range (0 to 100)
new_min <- 0
new_max <- 100

# Function to rescale raster values to a new range
# rescaleRaster <- function(raster_obj, new_min, new_max) {
#   # Get current minimum and maximum values of the raster
#   current_min <- minValue(raster_obj)
#   current_max <- maxValue(raster_obj)
#   
#   # Rescale the raster values
#   rescaled_raster <- raster_obj
#   rescaled_raster[] <- ((rescaled_raster - current_min) / (current_max - current_min)) * (new_max - new_min) + new_min
#   
#   return(rescaled_raster)
# }

# Function to scale raster values to a new range
scaleRaster <- function(raster_obj, new_min, new_max) {
  # Calculate the scaling factor
  scale_factor <- (new_max - new_min) / 254
  
  # Scale the raster values using calc()
  scaled_raster <- calc(raster_obj, function(x) (x * scale_factor) + new_min)
  
  return(scaled_raster)
}

# Rescale each raster
# Scale the raster values
zonation_uncon_rescaled <- scaleRaster(zonation_uncon_r_crop, new_min, new_max)
zonation_PA_rescaled <- scaleRaster(zonation_PA_r_crop, new_min, new_max)
zonation_PA_humanfoot_rescaled <- scaleRaster(zonation_PA_humanfoot_r_crop, new_min, new_max)

# Load the shapefile
state_shape <- st_read("A:/BB/masters-thesis/data/Sarawak.shp")
state_shape <- st_transform(state_shape, crs = crs(zonation_uncon_r_crop))

# Extract the boundary of the shapefile
state_border <- st_geometry(st_boundary(state_shape))

# Plotting
# Plotting raster with custom color palette
plot(zonation_uncon_rescaled, breaks=cuts, col = pal(7)) #plot with defined breaks
# plot(zonation_uncon_r)
# Plot the state boundary on top
plot(state_border, add = TRUE, border = NA, col = "white", lwd = 2)

# Export as PNG file
# Unconstrained Zonation output
png("A:/BB/masters-thesis/Zonation/output_zonation_uncon_stateborder.png", width = 1600, height = 1200,
    # units = "px", res = 300
    )  # Adjust dimensions and resolution as needed
plot(zonation_uncon_rescaled, breaks=cuts, col = pal(7))
plot(state_border, add = TRUE, border = NA, col = "white", lwd = 2)
dev.off()

png("A:/BB/masters-thesis/Zonation/output_zonation_uncon.png", width = 1600, height = 1200)
plot(zonation_uncon_rescaled, breaks=cuts, col = pal(7))
dev.off()

# Zonation PA output
png("A:/BB/masters-thesis/Zonation/output_zonation_PA_stateborder.png", width = 1600, height = 1200,
    # units = "px", res = 300
)  # Adjust dimensions and resolution as needed
plot(zonation_PA_rescaled, breaks=cuts, col = pal(7))
plot(state_border, add = TRUE, border = NA, col = "white", lwd = 2)
dev.off()

png("A:/BB/masters-thesis/Zonation/output_zonation_PA.png", width = 1600, height = 1200)

plot(zonation_PA_rescaled, breaks=cuts, col = pal(7))
dev.off()

# Zonation PA-HumanFootprint output
png("A:/BB/masters-thesis/Zonation/output_zonation_PA_humanfoot_stateborder.png", width = 1600, height = 1200,
    # units = "px", res = 300
)  # Adjust dimensions and resolution as needed
plot(zonation_PA_humanfoot_rescaled, breaks=cuts, col = pal(7))
plot(state_border, add = TRUE, border = NA, col = "white", lwd = 2)
dev.off()

png("A:/BB/masters-thesis/Zonation/output_zonation_PA_humanfoot.png", width = 1600, height = 1200)

plot(zonation_PA_humanfoot_rescaled, breaks=cuts, col = pal(7))
dev.off()