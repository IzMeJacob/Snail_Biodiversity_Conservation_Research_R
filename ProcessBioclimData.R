library(raster)
library(geodata)
library(terra)  # This is optional if you are using raster package only

# Create directories if they don't exist
if (!file.exists("data/bioclim/future")) dir.create("data/bioclim/future")
if (!file.exists("data/bioclim/future/wc2.1_2.5m")) {
  cmip6_world("ACCESS-CM2", "585", "2021-2040", var="bioc", res=2.5, path="data/bioclim/future")
}

# List all .tif files in the directory
future_clim_files <- list.files("data/bioclim/future/wc2.1_2.5m", pattern = ".tif$", full.names = TRUE)

# Create a RasterBrick object from the TIFF file
# future_clim_stack <- brick(future_clim_files)
future_clim_stack <- stack(future_clim_files)

# Get the number of bands in the RasterBrick
num_bands <- nlayers(future_clim_stack)

# List the names of the bands (if not set, they may be automatically numbered)
band_names <- names(future_clim_stack)
print(band_names)

# Create a vector of new names (bio1, bio2, ..., bio19)
new_names <- paste0("bio", 1:num_bands)

# Rename the bands in the RasterBrick
names(future_clim_stack) <- new_names

# Verify the new names
print(names(future_clim_stack))

# Optionally, plot the bands with new names
par(mfrow = c(ceiling(sqrt(num_bands)), ceiling(sqrt(num_bands))), mar = c(1, 1, 1, 1))
for (i in 1:num_bands) {
  plot(future_clim_stack[[i]], main = new_names[i])
}
