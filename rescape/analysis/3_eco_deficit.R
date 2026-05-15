## grey areas or sparsley vegetated
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)
source("analysis/utils.R")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


####---- input files ----

stud_area<-st_read("data/stud_site.gpkg")
ec_connectivity <- get_newest_tif("output/connectivity")
ec_connectivity<-terra::rast(ec_connectivity)

es_status <- get_newest_tif("output/wssi","pot_es_rest")

es_status<-terra::rast(es_status)
es_status<-rescale01(es_status)
target_crs<-st_crs(stud_area)

ec_connectivity <- resample(ec_connectivity, es_status, "mean")
ec_connectivity<-rescale01(ec_connectivity)

plot(c(ec_connectivity,es_status))

### this is the correct one ( "ecological deficit")
#High values indicate:

#poor ES condition
#poor connectivity
#degraded landscapes

#Low values indicate: healthy and connected systems
eco_def<-1/(es_status*ec_connectivity)
plot(log(eco_def))

def_name <- paste0("output/eco_deficit/eco_def_", timestamp, ".tif")
writeRaster(eco_def,
            def_name,
            overwrite=TRUE)
