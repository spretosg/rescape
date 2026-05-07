## mean es
library(sf)
library(dplyr)
library(terra)
## create mean es raster

cult_files <- list.files("data/es/cult", full.names = TRUE)
es_stack <- terra::rast(cult_files)
# Assign WGS84 correctly
terra::crs(es_stack) <- "EPSG:4326"
es_cult<- terra::mean(es_stack, na.rm = T)
plot(es_cult)
writeRaster(es_cult,"output/es/cult_mean.tif")


eco_files <- list.files("data/es/eco", full.names = TRUE)
es_stack <- terra::rast(eco_files)
# Assign WGS84 correctly
terra::crs(es_stack) <- "EPSG:4326"
es_eco<- terra::mean(es_stack, na.rm = T)
plot(es_eco)
writeRaster(es_eco,"output/es/eco_mean.tif")


prov_files <- list.files("data/es/prov", full.names = TRUE)
es_stack <- terra::rast(prov_files)
# Assign WGS84 correctly
terra::crs(es_stack) <- "EPSG:4326"
es_prov<- terra::mean(es_stack, na.rm = T)
plot(es_prov)
writeRaster(es_eco,"output/es/prov_mean.tif")


reg_files <- list.files("data/es/reg", full.names = TRUE)
es_stack <- terra::rast(reg_files)
# Assign WGS84 correctly
terra::crs(es_stack) <- "EPSG:4326"
es_reg<- terra::mean(es_stack, na.rm = T)
plot(es_reg)
writeRaster(es_reg,"output/es/reg_mean.tif")
