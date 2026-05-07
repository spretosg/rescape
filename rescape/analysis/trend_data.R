## degradation
# using trend earth to calculate three sub indicators of degradation
#trend raster from trend.earth

#a base raster
r<-rast("data/es/cult/recr_mean.tif")
trend<-terra::rast("data/es_trend_dat.tif")

#1) LULC change
trend_lulc<-trend[[14]]
trend_lulc <- resample(trend_lulc, r, "min")
plot(trend_lulc)

## ev LULC transitions
trans_lulc<-trend[[15]]
trans_lulc <- resample(trans_lulc, r, "min")
neg_trend<-c(12,13,6)
trans_filtered <- ifel(trans_lulc %in% neg_trend, trans_lulc, 0)
plot(trans_filtered)
trans_filtered[trans_filtered > 0] <- -1

writeRaster(trans_filtered,
            "output/verif_lulc_trans.tif",
            overwrite=TRUE)

#2) Productivity (NDVI) trend
trend_NDVI<-trend[[22]]
trend_NDVI[trend_NDVI == -32768] <- NA

rcl <- matrix(c(
  1,-1,
  2, -1,#stable
  3,0,
  4,0,
  5,1
), ncol = 2, byrow = TRUE)
trend_NDVI <- classify(trend_NDVI, rcl)
# trend_NDVI[trend_NDVI > 4] <- 1
# trend_NDVI[trend_NDVI == 4] <- 0
# trend_NDVI[trend_NDVI < 4] <- -1
trend_NDVI <- resample(trend_NDVI, r, "min")
plot(trend_NDVI)
# trend_NDVI_R <- resample(trend_NDVI, trend_lulc, "min")
# plot(trend_NDVI_R)

#3) carbon storage soil organic carbon trend
trend_soil<-trend[[19]]
trend_soil <- resample(trend_soil, r, "min")
plot(trend_soil)

trend_soil[trend_soil == -32768] <- NA
# now r is % in soil trend we should reclass as +1 increase -1 decrease?
trend_soil[trend_soil > 0] <- 1
trend_soil[trend_soil < 0] <- -1
plot(trend_soil)

# trend_soil<-terra::rast("data/test.grd")

# composition
comp_trend<-trend_soil+trend_NDVI+trans_filtered
plot(comp_trend)


writeRaster(comp_trend,
            "output/composed_trend.tif",
            overwrite=TRUE)
