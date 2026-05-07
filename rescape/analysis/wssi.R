## ES weighted sum similarity index WSSI

# Weighted Sum Similarity Index (WSSI)
# Using ecosystem service rasters with terra

library(terra)

#-----------------------------------------------------------
# 1. Load ecosystem service rasters (not include economic)
#-----------------------------------------------------------



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

#-----------------------------------------------------------
# 3. Weighted sum of ES supply
#-----------------------------------------------------------

weighted_es <- es * w

weighted_sum <- app(weighted_es, sum, na.rm=TRUE)
writeRaster(weighted_sum,
            "output/es_sum_weighted.tif",
            overwrite=TRUE)


#-----------------------------------------------------------
# 4. Total ES supply per pixel
#-----------------------------------------------------------

total_es <- app(es, sum, na.rm=TRUE)

# Avoid division by zero
total_es[total_es == 0] <- NA

#-----------------------------------------------------------
# 5. Relative proportion of each ES
#    ES'ij = ESij / totalESj
#-----------------------------------------------------------

es_prop <- es / total_es
plot(es_prop)
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

distance_raster <- app(es_prop, euclidean_fun)

#-----------------------------------------------------------
# 7. Similarity index
#    similarity = 1 - distance
#-----------------------------------------------------------

similarity <- 1 - distance_raster

# Optional normalization to 0-1
#similarity <- clamp(similarity, 0, 1)
plot(similarity)
#-----------------------------------------------------------
# 8. Weighted Sum Similarity Index (WSSI)
#-----------------------------------------------------------

wssi <- weighted_sum * similarity

names(wssi) <- "WSSI"

#-----------------------------------------------------------
# 9. Identify reference sites
#    Pixels above 95th percentile
#-----------------------------------------------------------

p95 <- global(wssi, quantile,
              probs=0.95,
              na.rm=TRUE)[1,1]

reference_sites <- wssi >= p95
plot(reference_sites)
#-----------------------------------------------------------
# 10. Mean WSSI of reference pixels
#-----------------------------------------------------------

reference_mean <- global(
  mask(wssi, reference_sites),
  mean,
  na.rm=TRUE
)

print(reference_mean)

#-----------------------------------------------------------
# 11. Effectiveness
#-----------------------------------------------------------
rest_eff<- as.numeric(reference_mean[1,1])-wssi

plot(rest_eff)
#
writeRaster(wssi,
            "output/wssi.tif",
            overwrite=TRUE)

writeRaster(rest_eff,
            "output/effectiveness.tif",
            overwrite=TRUE)

