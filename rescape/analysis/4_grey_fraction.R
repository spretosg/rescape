library(sf)
library(terra)
library(exactextractr)
library(dplyr)

source("analysis/utils.R")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
stud_area<-st_read("data/stud_site.gpkg")

target_crs<-st_crs(stud_area)

# ec_connectivity<-terra::rast("output/ec_connect_ini.tif")
eco_def <- get_newest_tif("output/eco_deficit")
eco_def<-terra::rast(eco_def)

#### grey area
grey_a<-st_read("data/grey.gpkg")
sparse_a<-st_read("data/sparse_vegetation.gpkg")


grey_a<-sf::st_transform(grey_a,target_crs)
sparse_a<-sf::st_transform(sparse_a,target_crs)



results <- exact_extract(
  eco_def,
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

r_grey <- rast(eco_def)
values(r_grey) <- NA
r_grey[df_sum$cell] <- df_sum$fraction
plot(r_grey)


results <- exact_extract(
  eco_def,
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

r_sparse <- rast(eco_def)
values(r_sparse) <- NA
r_sparse[df_sum$cell] <- df_sum$fraction
plot(r_sparse)

sum_grey<-sum(r_sparse,r_grey)
plot(sum_grey)
grey_name <- paste0("data/sum_grey_", timestamp, ".tif")

writeRaster(sum_grey,
            grey_name,
            overwrite=TRUE)
