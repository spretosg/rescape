## comparision with rest map norconsult

library(terra)
library(sf)
source("analysis/utils.R")

eco_def <- get_newest_tif("output/eco_deficit")
eco_def<-terra::rast(eco_def)

restoration_ext<-st_read("data/rest_areal_norconsult/rest_area_incl_grey.gpkg")
restoration_ext <- restoration_ext %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_union()

restoration_ext <- restoration_ext %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")
restoration_ext<-st_transform(restoration_ext,crs(eco_def))

# convert to terra vector
poly <- vect(as(restoration_ext, "Spatial"))

# values INSIDE polygon
inside_vals <- mask(eco_def, poly)
inside_vals[is.infinite(inside_vals)] <- NA
outside_vals <- mask(eco_def, poly, inverse = TRUE)
outside_vals[is.infinite(outside_vals)] <- NA

inside_stats <- global(
  inside_vals,
  fun = c("mean", "sd"),
  na.rm = TRUE
)

outside_stats <- global(
  outside_vals,
  fun = c("mean", "sd"),
  na.rm = TRUE
)

deficit_stats<-data.frame(
  region = c("in_norcons_pot_rest", "out_norcons_pot_rest"),
  mean = c(
    inside_stats[1, "mean"],
    outside_stats[1, "mean"]
  ),
  sd = c(
    inside_stats[1, "sd"],
    outside_stats[1, "sd"]
  )
)

#### trend
rest_pot_trend <- get_newest_tif("output/rest_prio","ndvi")
rest_pot_trend<-terra::rast(rest_pot_trend)

## rest_pot_based on NDVI inside and outside norconsult map
inside_vals <- mask(rest_pot_trend, poly)
inside_vals[is.infinite(inside_vals)] <- NA
outside_vals <- mask(rest_pot_trend, poly, inverse = TRUE)
outside_vals[is.infinite(outside_vals)] <- NA

inside_stats <- global(
  inside_vals,
  fun = c("mean", "sd"),
  na.rm = TRUE
)

outside_stats <- global(
  outside_vals,
  fun = c("mean", "sd"),
  na.rm = TRUE
)

rest_pot_trend_stats<-data.frame(
  region = c("inside", "outside"),
  mean = c(
    inside_stats[1, "mean"],
    outside_stats[1, "mean"]
  ),
  sd = c(
    inside_stats[1, "sd"],
    outside_stats[1, "sd"]
  )
)

###
