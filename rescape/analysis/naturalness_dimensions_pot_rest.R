##
library(terra)
library(sf)

rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

stud_area<-st_read("data/stud_site.gpkg")
target_crs<-st_crs(stud_area)

#raster
lulc<-terra::rast("data/lulc.tif")
#lulc<-as.factor(lulc)
lulc[lulc == 0] <- NA
lulc <- trunc(lulc / 100)

ec<-terra::rast("data/ec.tif")


# con<-terra::rast("data/connectivity.tif")
# con<-rescale01(con)
# terra::crs(con) <- "EPSG:25832"
# con<-terra::project(con,crs(ec))
#
# spec<-terra::rast("data/spec_rich.tif")
# spec<-terra::project(spec,crs(ec))
# spec<-rescale01(spec)
#
# trend<-terra::rast("output/composed_trend.tif")
#
# #es_w<-terra::rast("output/es_sum_weighted.tif")
# #es_reg<-terra::rast("output/es/reg_mean.tif")
# es_cult<-terra::rast("output/es/cult_mean.tif")
# es_cult<-rescale01(es_cult)
# es_prov<-terra::rast("output/es/prov_mean.tif")
# erosion<-terra::rast("data/es/reg/erosion_mean.tif")
# flood<-terra::rast("data/es/reg/flood_mean.tif")
# # habitat<-terra::rast("data/es/reg/habitat_mean.tif")
#
# es_reg<-mean(erosion,flood)
# es_reg<-rescale01(es_reg)
#
# es_w<-mean(es_reg,es_cult)
# plot(es_w)
#
# #unweighted mean excluding mat and agri
#
# es_w <- rescale01(es_w)
#
# #wssi<-terra::rast("output/wssi.tif")
#
# ## resample
# con <- resample(con, es_w)
# con <- rescale01(con)
# ec <- resample(ec, es_w)
# ec <- rescale01(ec)
# spec <- resample(spec, es_w)
# spec <- rescale01(spec)
# # wssi <- resample(wssi, es_w)
# # wssi <- rescale01(wssi)
#
# dim<-c(ec,es_w)
# names(dim) <- c(
#   #"connectivity",
#   "e_condition",
#   "e_services"
#   #"species_rich"
# )
# plot(dim)
#
# w <- c(
#   # connectivity  = 0.5,
#   e_condition  = 0.5,
#   e_services  = 0.5
# )
#
#
# total_dim <- app(dim, sum)
# plot(total_dim)
#
# # Avoid division by zero
# total_dim[total_dim == 0] <- NA
#
# dim_prop <- dim / total_dim
# plot(dim_prop)
#
# # Function applied pixel-wise
# euclidean_fun <- function(x) {
#
#   # x = proportional ES values for one pixel
#   if(any(is.na(x))) return(NA)
#
#   sqrt(sum((w-x)^2))
# }
#
# # dist_simple <- function(x) {
# #
# #   # x = proportional ES values for one pixel
# #   if(any(is.na(x))) return(NA)
# #
# #   sum(abs(w - x))
# # }
#
# distance_raster <- app(dim_prop, euclidean_fun)
# plot(distance_raster)
# similarity <- 1 - distance_raster
#
# plot(similarity)
#
# weighted_dim <- dim * w
# w_sum_dim <- app(weighted_dim, sum)
# w_sum_dim[w_sum_dim == 0] <- NA
# plot(w_sum_dim)
# # wssi_dim <- w_sum_dim * similarity
# # plot(wssi_dim)
#
# # writeRaster(wssi_dim,
# #             "output/effectiveness.tif",
# #             overwrite=TRUE)
#
# # p95 <- global(wssi_dim, quantile,
# #               probs=0.95,
# #               na.rm=TRUE)[1,1]
# #
# # reference_sites <- wssi_dim >= p95
#
# p95 <- global(w_sum_dim, quantile,
#               probs=0.95,
#               na.rm=TRUE)[1,1]
#
# reference_sites <- w_sum_dim >= p95
#
# plot(reference_sites)
#
# r_clamped <- clamp(w_sum_dim, lower=p95, values=FALSE)
# # reference_mean <- global(
# #   mask(wssi_dim, reference_sites),
# #   mean,
# #   na.rm=TRUE
# # )
# reference_mean <- global(
#   r_clamped,
#   na.rm=T
# )
#
# #restoration effectiveness
# rest_eff<- as.numeric(reference_mean[1,1])-w_sum_dim
#
#
# rest_eff<-rest_eff*-1*trend
#
# plot(rest_eff)
#
# restoration_priority <- (1 - ec) * (-trend)
#
# writeRaster(restoration_priority,
#             "output/effectiveness9.tif",
#             overwrite=TRUE)

#-----------------------------------------------------------
# 12. Effectiveness EC
#-----------------------------------------------------------
# ec<-terra::rast("data/ec.tif")
lulc <- project(lulc, crs(ec), method="near")
ec<-resample(ec,lulc,"mean")
lulc<-as.factor(lulc)
ec<-rescale01(ec)


# calculate 95th percentile per LULC class
p95_by_class <- zonal(
  ec,
  lulc,
  fun = function(x, ...) {
    quantile(x, probs = 0.95, na.rm = TRUE)
  }
)

print(p95_by_class)

# create empty raster
reference_sites <- ec
values(reference_sites) <- NA

# loop through classes
for(i in 1:nrow(p95_by_class)) {

  cls <- p95_by_class$lulc[i]
  thr <- p95_by_class$ec[i]

  mask <- lulc == cls & ec >= thr

  # keep original ec values only in mask
  reference_sites[mask] <- ec[mask]
}

plot(reference_sites)

reference_mean <- zonal(
  reference_sites,
  lulc,
  fun="mean",
  na.rm=TRUE
)

rest_pot_ec <- ec
values(rest_pot_ec) <- NA

# loop through classes
for(i in 1:nrow(reference_mean)) {

  cls <- reference_mean$lulc[i]
  ref_mean  <- reference_mean$ec[i]


  # pixels belonging to this class
  class_mask <- lulc == cls

  # class-specific restoration potential
  rest_pot_ec[class_mask] <- ref_mean  - ec[class_mask]
}

plot(rest_pot_ec)
writeRaster(rest_pot_ec,
            "output/ec_status.tif",
            overwrite=TRUE)

### trend
trend<-terra::rast("output/composed_trend.tif")
trend <- resample(trend, rest_pot_ec, method="bilinear")

rest_eff_ec <-  rest_pot_ec * (-trend)

plot(rest_eff_ec)

writeRaster(rest_eff_ec,
            "output/ec_effectiveness.tif",
            overwrite=TRUE)
