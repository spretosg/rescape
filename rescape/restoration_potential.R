## grey areas or sparsley vegetated
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)

stud_area<-st_read("data/stud_site.gpkg")

target_crs<-st_crs(stud_area)

ec_connectivity<-terra::rast("output/ec_connect_ini.tif")
trend<-terra::rast("data/es_trend_dat.tif")
# NDVI_trend<-trend[[22]]
NDVI_trend<-trend[[4]]
plot(NDVI_trend)

es_status<-terra::rast("output/es_status.tif")


### NDVI
NDVI_trend[NDVI_trend == -32768] <- NA

rcl <- matrix(c(
  -3,-1,#declining
  -2, -1,#declining
  -1,-1,#declining
  0,0,#stable
  1,0,#increasing
  2,1,#increasing
  3,1#increasing
), ncol = 2, byrow = TRUE)
NDVI_trend <- classify(NDVI_trend, rcl)

NDVI_trend <- resample(NDVI_trend, es_status, "min")
plot(NDVI_trend)

ec_connectivity <- resample(ec_connectivity, es_status, "mean")
ec_connectivity<-rescale01(ec_connectivity)





rest_effect<-1/(es_status*ec_connectivity)
plot(rest_effect)

writeRaster(rest_effect,
            "output/rest_effect.tif",
            overwrite=TRUE)



### negarest_effect


#### grey areas

grey_a<-st_read("data/grey.gpkg")
sparse_a<-st_read("data/sparse_vegetation.gpkg")

# grey_a$area<-as.numeric(st_area(grey_a))
# sparse_a$area<-as.numeric(st_area(sparse_a))

grey_a<-sf::st_transform(grey_a,target_crs)
sparse_a<-sf::st_transform(sparse_a,target_crs)

rest_pot<-terra::rast("output/ec_effectiveness.tif")
rest_pot_es<-terra::rast("output/es_effectiveness.tif")
trans<-terra::project(rest_pot,"EPSG:25832")


results <- exact_extract(
  rest_pot,
  grey_a,
  coverage_area = T,
  include_area = T,
  include_cell = T
)

df <- bind_rows(results)

df_sum <- df %>%
  group_by(cell) %>%
  summarise(coverage_area = sum(coverage_area, na.rm=TRUE),
            area = min(area),
            fraction = sum(coverage_area, na.rm=TRUE)/ min(area))

r_grey <- rast(rest_pot)
values(r_grey) <- NA
r_grey[df_sum$cell] <- df_sum$fraction
plot(r_grey)

rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}



results <- exact_extract(
  rest_pot,
  sparse_a,
  coverage_area = T,
  include_area = T,
  include_cell = T
)

df <- bind_rows(results)

df_sum <- df %>%
  group_by(cell) %>%
  summarise(coverage_area = sum(coverage_area, na.rm=TRUE),
            area = min(area),
            fraction = sum(coverage_area, na.rm=TRUE)/ min(area))

r_sparse <- rast(rest_pot)
values(r_sparse) <- NA
r_sparse[df_sum$cell] <- df_sum$fraction
plot(r_sparse)

sum_grey<-sum(r_sparse,r_grey)
writeRaster(sum_grey,
            "output/sum_grey.tif",
            overwrite=TRUE)

sum_grey<-grey
rest_effect<-log(rest_effect)
plot(rest_effect)

rest_pot<-rescale01(rest_pot)
rest_pot_es<-rescale01(rest_pot_es)
NDV

rest_pot_grey<-rest_effect*sum_grey

NDVI_trend<-resample(NDVI_trend,rest_effect,"min")
rest_pot_trend<-rest_effect*NDVI_trend
rest_pot_trend[rest_pot_trend>-1]<-NA
rest_pot_trend<--1*rest_pot_trend
rest_pot_trend<-rescale01(rest_pot_trend)
rest_pot_grey<-rescale01(rest_pot_grey)

plot(rest_pot_grey)
writeRaster(rest_pot_grey,
            "output/rest_pot_grey.tif",
            overwrite=TRUE)

writeRaster(rest_pot_trend,
            "output/rest_pot_trend.tif",
            overwrite=TRUE)
