## ES weighted sum similarity index WSSI

# Weighted Sum Similarity Index (WSSI)
# Using ecosystem service rasters with terra

library(terra)
library(sf)

rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

#-----------------------------------------------------------
# 1. Load ecosystem service rasters (not include economic)
#-----------------------------------------------------------
trend<-terra::rast("output/composed_trend.tif")
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


# es_files <- c(
#   "output/es/cult_mean.tif",
#   "output/es/prov_mean.tif",
#   "output/es/reg_mean.tif"
# )

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

# names(es) <- c(
#   "cult",
#   "prov",
#   "reg"
# )



#-----------------------------------------------------------
# 2. Stakeholder weights
#    (must sum to 1)
#-----------------------------------------------------------

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
# w <- c(
#   cult  = 0.35,
#   prov = 0.25,
#   reg   = 0.4
# )

#-----------------------------------------------------------
# 3. Weighted sum of ES supply
#-----------------------------------------------------------

weighted_es <- es * w

weighted_sum <- app(weighted_es, sum)
plot(weighted_sum)
writeRaster(weighted_sum,
            "output/es_sum_weighted.tif",
            overwrite=TRUE)


#-----------------------------------------------------------
# 4. Total ES supply per pixel
#-----------------------------------------------------------

total_es <- app(es, sum) ## !! if na.rm = T produces wrong total values!!

# Avoid division by zero
total_es[total_es == 0] <- NA

#-----------------------------------------------------------
# 5. Relative proportion of each ES
#    ES'ij = ESij / totalESj
#-----------------------------------------------------------

es_prop <- es / total_es
# plot(es_prop)
#-----------------------------------------------------------
# 6. Euclidean distance between:
#    stakeholder demand weights
#    vs actual ES proportions
#-----------------------------------------------------------

# Function applied pixel-wise
euclidean_fun <- function(x) {

  # x = proportional ES values for one pixel
  if(any(is.na(x))) return(NA)

  sqrt(sum((w-x)^2))
}

# dist_simple <- function(w,x) {
#
#   # x = proportional ES values for one pixel
#   if(any(is.na(x))) return(NA)
#
#   sum(w - x)
# }


distance_raster <- app(es_prop, euclidean_fun)
# f<-app(c(w,es_prop), dist_simple)
# plot(distance_raster)
#-----------------------------------------------------------
# 7. Similarity index
#    similarity = 1 - distance
#-----------------------------------------------------------

similarity <- 1 - distance_raster
# dmax <- sqrt((1-0.25)^2 + 3*(0-0.25)^2)
#
# similarity <- 1 - (distance_raster / dmax)
# Optional normalization to 0-1
#similarity <- clamp(similarity, 0, 1)
# plot(similarity)
#-----------------------------------------------------------
# 8. Weighted Sum Similarity Index (WSSI)
#-----------------------------------------------------------

wssi <- weighted_sum * similarity
wssi[wssi <= 0.01] <- NA
plot(wssi)
names(wssi) <- "WSSI"

writeRaster(wssi,
            "output/wssi.tif",
            overwrite=TRUE)

## wssi per lulc
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

writeRaster(rest_pot_es,
            "output/es_status.tif",
            overwrite=TRUE)


trend <- resample(trend, rest_pot_es, method="bilinear")

trend <- crop(trend, rest_pot_es)

rest_eff_es<-rest_pot_es* (-trend)


plot(rest_eff_es)
#


writeRaster(rest_eff_es,
            "output/es_effectiveness.tif",
            overwrite=TRUE)



