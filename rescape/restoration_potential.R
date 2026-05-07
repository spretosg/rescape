## grey areas or sparsley vegetated
library(sf)
library(terra)
library(exactextractr)

stud_area<-st_read("data/stud_site.gpkg")
target_crs<-st_crs(stud_area)

grey_a<-st_read("data/grey.gpkg")
sparse_a<-st_read("data/sparse_vegetation.gpkg")

# grey_a$area<-as.numeric(st_area(grey_a))
# sparse_a$area<-as.numeric(st_area(sparse_a))

grey_a<-sf::st_transform(grey_a,target_crs)
sparse_a<-sf::st_transform(sparse_a,target_crs)

rest_pot<-terra::rast("output/effectiveness.tif")
trans<-terra::project(rest_pot,"EPSG:25832")


# Convert sf -> SpatVector
# grey_a<-grey_a%>%st_zm()%>%st_make_valid(grey_a)
# v_grey <- vect(grey_a)
#
# v_sparse<-vect(sparse_a)


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

rest_pot<-rescale01(rest_pot)


eff_potential<-rest_pot*sum_grey

plot(eff_potential)
writeRaster(eff_potential,
            "output/rest_pot_est.tif",
            overwrite=TRUE)
