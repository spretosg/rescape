## ES weighted sum similarity index WSSI

# Weighted Sum Similarity Index (WSSI)
# Using ecosystem service rasters with terra

library(terra)
library(sf)
source("analysis/utils.R")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

####---- input data ----
#load lulc and ES rasters

lulc<-terra::rast("data/lulc.tif")
#lulc<-as.factor(lulc)
lulc[lulc == 0] <- NA
lulc <- trunc(lulc / 100)

es_files <- c(
  "data/es/cult/aest_mean.tif",
  "data/es/cult/recr_mean.tif",
  "data/es/cult/sense_mean.tif",
  "data/es/prov/wild_hunt_mean.tif",
  "data/es/prov/wild_plant_mean.tif",
  "data/es/reg/erosion_mean.tif",
  "data/es/reg/flood_mean.tif",
  "data/es/reg/habitat_mean.tif",
  "data/es/eco/farm_mean.tif",
  "data/es/eco/mat_mean.tif"
)

es <- rast(es_files)
for(i in 1:nlyr(es)) {
  r <- es[[i]]

  mn <- global(r, min, na.rm=TRUE)[1,1]
  mx <- global(r, max, na.rm=TRUE)[1,1]

  es[[i]] <- (r - mn) / (mx - mn)
}


names(es) <- c(
  "aest",
  "recr",
  "sense",
  "wild_hunt",
  "wild_plant",
  "erosion",
  "flood",
  "habitat",
  "farm",
  "mat"
)

#stakeholder weights from AHP of PPGIS
w <- c(
  aest  = 0.09,
  recr = 0.092,
  sense   = 0.13,
  wild_hunt = 0.098,
  wild_plant = 0.098,
  erosion = 0.115,
  flood = 0.1,
  habitat = 0.079,
  farm = 0.088,
  mat = 0.11

)
####---- calculate WSSI ----
# based on Silva et al. 2023 Prioritizing areas for ecological restoration: A participatory approach based on cost-effectiveness
#-----------------------------------------------------------
#  Weighted sum of ES supply
#-----------------------------------------------------------

weighted_es <- es * w

weighted_sum <- app(weighted_es, sum)
# plot(weighted_sum)
w_name <- paste0("output/wssi/es_sum_weighted_", timestamp, ".tif")

writeRaster(weighted_sum,
            w_name,
            overwrite=TRUE)


#-----------------------------------------------------------
# Total ES supply per pixel
#-----------------------------------------------------------

total_es <- app(es, sum) ## !! if na.rm = T produces wrong total values!!

# Avoid division by zero
total_es[total_es == 0] <- NA

#-----------------------------------------------------------
# Relative proportion of each ES
#    ES'ij = ESij / totalESj
#-----------------------------------------------------------

es_prop <- es / total_es

#-----------------------------------------------------------
# Euclidean distance between:
#    stakeholder demand weights
#    vs actual ES proportions
#-----------------------------------------------------------

distance_raster <- app(es_prop, euclidean_fun)

#-----------------------------------------------------------
#  Similarity index
#    similarity = 1 - distance
#-----------------------------------------------------------

similarity <- 1 - distance_raster

#-----------------------------------------------------------
# Weighted Sum Similarity Index (WSSI)
#-----------------------------------------------------------

wssi <- weighted_sum * similarity
wssi[wssi <= 0.01] <- NA
plot(wssi)
names(wssi) <- "WSSI"

wssi_name <- paste0("output/wssi/wssi_", timestamp, ".tif")

writeRaster(wssi,
            wssi_name,
            overwrite=TRUE)

#-----------------------------------------------------------
# Deviance of WSSI from potential best areas per LULC
# pot rest based on ES multifuctionality
#-----------------------------------------------------------

lulc <- project(lulc, crs(wssi), method="near")
wssi<-resample(wssi,lulc,"mean")
lulc<-as.factor(lulc)


# calculate 95th percentile per LULC class
p95_by_class <- zonal(
  wssi,
  lulc,
  fun = function(x, ...) {
    quantile(x, probs = 0.95, na.rm = TRUE)
  }
)

print(p95_by_class)

# create empty raster
reference_sites <- wssi
values(reference_sites) <- NA

# loop through classes
for(i in 1:nrow(p95_by_class)) {

  cls <- i
  thr <- p95_by_class$WSSI[i]

  mask <- lulc == cls & wssi >= thr

  # keep original ec values only in mask
  reference_sites[mask] <- wssi[mask]
}

plot(reference_sites)

reference_mean <- zonal(
  reference_sites,
  lulc,
  fun="mean",
  na.rm=TRUE
)


rest_pot_es <- wssi
values(rest_pot_es) <- NA

# loop through classes
for(i in 1:nrow(reference_mean)) {

  cls <- i
  ref_mean  <- reference_mean$WSSI[i]


  # pixels belonging to this class
  class_mask <- lulc == cls

  # class-specific restoration potential
  rest_pot_es[class_mask] <- ref_mean  - wssi[class_mask]
}

plot(rest_pot_es)
es_pot_name <- paste0("output/wssi/pot_es_rest_", timestamp, ".tif")


writeRaster(rest_pot_es,
            es_pot_name,
            overwrite=TRUE)

#
# trend <- resample(trend, rest_pot_es, method="bilinear")
#
# trend <- crop(trend, rest_pot_es)
#
# rest_eff_es<-rest_pot_es* (-trend)



#






