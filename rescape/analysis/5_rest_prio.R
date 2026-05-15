library(sf)
library(terra)

source("analysis/utils.R")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

eco_def <- get_newest_tif("output/eco_deficit")
eco_def<-terra::rast(eco_def)
eco_def<-log(eco_def)
eco_def[is.infinite(values(eco_def))] <- NA
eco_def<-rescale01(eco_def)
plot(eco_def)

#from trends earth
ndvi_trend<-terra::rast("data/perform_trend_140526.tif")
ndvi_trend<-ndvi_trend/100
plot(ndvi_trend)

ndvi_trend <- resample(ndvi_trend, eco_def, "min")
plot(ndvi_trend)

## grey areas
sum_grey<-terra::rast("data/sum_grey_20260515_110317.tif")

rest_pot_grey<-eco_def*sum_grey
plot(rest_pot_grey)


rest_pot_trend<-eco_def*ndvi_trend
plot(rest_pot_trend)

rest_pot_trend[rest_pot_trend>0]<-NA
rest_pot_trend<--1*rest_pot_trend
rest_pot_trend<-rescale01(rest_pot_trend)
rest_pot_grey<-rescale01(rest_pot_grey)
file_grey<-paste0("output/rest_prio/rest_prio_grey_", timestamp, ".tif")

writeRaster(rest_pot_grey,
            file_grey,
            overwrite=TRUE)

file_trend<-paste0("output/rest_prio/rest_prio_ndvi_", timestamp, ".tif")

writeRaster(rest_pot_trend,
            file_trend,
            overwrite=TRUE)
