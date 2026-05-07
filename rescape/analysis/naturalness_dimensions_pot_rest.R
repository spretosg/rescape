##


rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

stud_area<-st_read("data/stud_site.gpkg")
target_crs<-st_crs(stud_area)

#raster
ec<-terra::rast("data/ec.tif")
crs(ec)
con<-terra::rast("data/connectivity.tif")
terra::crs(con) <- "EPSG:25832"
con<-terra::project(con,crs(ec))

spec<-terra::rast("data/spec_rich.tif")
spec<-terra::project(spec,crs(ec))

#es_w<-terra::rast("output/es_sum_weighted.tif")
es_reg<-terra::rast("output/es/reg_mean.tif")
es_cult<-terra::rast("output/es/cult_mean.tif")
es_prov<-terra::rast("output/es/prov_mean.tif")
es_w<-mean(es_reg,es_cult,es_prov)
plot(es_w)

#unweighted mean excluding mat and agri

es_w <- rescale01(es_w)

#wssi<-terra::rast("output/wssi.tif")

## resample
con <- resample(con, es_w)
con <- rescale01(con)
ec <- resample(ec, es_w)
ec <- rescale01(ec)
spec <- resample(spec, es_w)
spec <- rescale01(spec)
# wssi <- resample(wssi, es_w)
# wssi <- rescale01(wssi)

dim<-c(con,ec,spec,es_w)
names(dim) <- c(
  "connectivity",
  "e_condition",
  "species_rich",
  "e_services"
)
plot(dim)

w <- c(
  connectivity  = 0.25,
  e_condition  = 0.25,
  species_rich  = 0.25,
  e_services  = 0.25
)


total_dim <- app(dim, sum, na.rm=TRUE)
plot(total_dim)

# Avoid division by zero
total_dim[total_dim == 0] <- NA

dim_prop <- dim / total_dim
plot(dim_prop)

# Function applied pixel-wise
euclidean_fun <- function(x) {

  # x = proportional ES values for one pixel
  if(any(is.na(x))) return(NA)

  sqrt(sum((x-w)^2))
}

distance_raster <- app(dim_prop, euclidean_fun)
#plot(similarity)
# similarity <- 1 - distance_raster

weighted_dim <- dim * w
w_sum_dim <- app(weighted_dim, sum, na.rm=TRUE)
wssi_dim <- w_sum_dim * distance_raster
plot(wssi_dim)


p95 <- global(wssi_dim, quantile,
              probs=0.95,
              na.rm=TRUE)[1,1]

reference_sites <- wssi_dim >= p95
plot(reference_sites)


reference_mean <- global(
  mask(wssi_dim, reference_sites),
  mean,
  na.rm=TRUE
)

rest_eff<- as.numeric(reference_mean[1,1])-wssi_dim

plot(rest_eff)

writeRaster(rest_eff,
            "output/effectiveness.tif",
            overwrite=TRUE)
