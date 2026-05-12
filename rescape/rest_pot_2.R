plot(trend)

plot(weighted_es)

-trend = stability

connectivity*state*stability

1= preservation
0= do nothing

remove upper and lower 25% from result

library(terra)
rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

ec_connectivity<-terra::rast("output/ec_connect_ini.tif")
trend<-terra::rast("data/es_trend_dat.tif")
# NDVI_trend<-trend[[22]]
NDVI_trend<-trend[[4]]
#
#
# test_NDVI<-trend[[3]]
# test_NDVI[test_NDVI == -32768] <- NA
# test_NDVI[test_NDVI < -1000] <- NA
# test_NDVI[test_NDVI > 1000] <- NA
#
# test_NDVI2<-trend[[4]]
# a<-c(NDVI_trend,test_NDVI2)
# plot(a)

es_status<-terra::rast("output/es_status.tif")


### NDVI
NDVI_trend[NDVI_trend == -32768] <- NA

rcl <- matrix(c(
  -3,1,#declining
  -2, 0.6,#declining
  -1,0.3,#declining
  0,0,#stable
  1,-1,#increasing
  2,-1,#increasing
  3,-1#increasing
), ncol = 2, byrow = TRUE)
NDVI_trend <- classify(NDVI_trend, rcl)
# trend_NDVI[trend_NDVI > 4] <- 1
# trend_NDVI[trend_NDVI == 4] <- 0
# trend_NDVI[trend_NDVI < 4] <- -1
NDVI_trend <- resample(NDVI_trend, es_status, "max")
plot(NDVI_trend)

ec_connectivity <- resample(ec_connectivity, es_status, "mean")
ec_connectivity<-rescale01(ec_connectivity)
es_status<-rescale01(es_status)

rest_pot<-es_status*ec_connectivity*NDVI_trend
plot(rest_pot)
rest_pot[rest_pot <=0] <- NA

writeRaster(rest_pot,
            "output/REST_POT_NEW2.tif",
            overwrite=TRUE)
