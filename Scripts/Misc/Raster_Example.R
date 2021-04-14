library(raster)
library(rasterVis)

setwd("/Volumes/GoogleDrive/My Drive/Teaching/2021/Data_Visualization/R/DataVisEss")

r_files <- list.files(path = "Geodata")


ndvi_1 <- raster(file.path("Geodata", r_files[2]))
names(ndvi_1) = "Wet_Season"
ndvi_2 <- raster(file.path("Geodata", r_files[3]))
names(ndvi_2) = "Dry_Season"

levelplot(ndvi_1, margin=FALSE)
levelplot(ndvi_2, margin=FALSE)

ndvi_1_2 <- stack(ndvi_1, ndvi_2)

levelplot(ndvi_1_2)

ndvi_mean <- mean(ndvi_1_2)
levelplot(ndvi_mean, margin = FALSE)

hist(ndvi_1_2)
density(ndvi_1_2)
