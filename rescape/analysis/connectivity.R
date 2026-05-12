library(circuitscaper)
#cs_install_julia()
Sys.which("julia")
system("julia --version")

rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

ec<-terra::rast("data/ec.tif")
ec<-rescale01(ec)

rest_pot_ec<-terra::rast(
  "output/rest_pot_ec.tif"
)

rest_pot_es<-terra::rast(
  "output/rest_pot_es.tif"
)

con_bird<-terra::rast(
  "data/connectivity.tif"
)
plot(con_bird)






lulc<-terra::rast("data/lulc.tif")
#lulc<-as.factor(lulc)
lulc[lulc == 0] <- NA
lulc <- trunc(lulc / 100)
lulc<-project(lulc,crs(resistance_ini))

# ensure same geometry
lulc <- resample(lulc, resistance_ini, method = "near")



factors <- data.frame(
  class = c(1, 2, 3, 4, 5),
  factor = c(0.1, 1, 3, 4,4)
)

# --- Reclassify LULC raster to factor raster ---
factor_raster <- classify(
  lulc,
  rcl = as.matrix(factors)
)

# --- Multiply original raster by factors ---
r_adjusted <- ec * factor_raster
plot(r_adjusted)

# resistance_ini<-1/log(r_adjusted )
resistance_ini <- 1/r_adjusted

r_coarse_ini <- aggregate(resistance_ini, fact = 2)



r_coarse_ini[r_coarse_ini <= 0] <- 0.01
r_coarse_ini[is.infinite(values(r_coarse_ini))] <- NA
plot(r_coarse_ini)


# calculate moving window connectivity based on ecosystem condition in landscape
start<-Sys.time()
mw_result_ini <- os_run(r_coarse_ini, radius = 30, block_size = 5)
plot(mw_result_ini$normalized_current)
print(Sys.time()-start)




norm_conn<-mw_result_ini$normalized_current

norm_conn[norm_conn < 0.95] <- -1
plot(norm_conn)

writeRaster(mw_result_ini$normalized_current,
            "output/ec_connect_ini4.tif",
            overwrite=TRUE)




## resistance new (if restored)

# important sites for restoration ec
rest_pot_ec <- resample(rest_pot_ec, ec)
rest_pot_es <- resample(rest_pot_es, ec)

# plot(rest_pot_ec*rest_pot_es)

ec_p <- global(rest_pot_ec, quantile,
              probs=0.75,
              na.rm=TRUE)[1,1]

reference_sites_ec <- rest_pot_ec >= ec_p
plot(reference_sites_ec)

es_p <- global(rest_pot_es, quantile,
                 probs=0.75,
                 na.rm=TRUE)[1,1]

reference_sites_es <- rest_pot_es >= es_p
plot(reference_sites_es)

# overlap mask
reference_overlap <- reference_sites_ec & reference_sites_es

plot(reference_overlap)
ec_new <- ec

# increase EC values by 30% where overlap occurs
ec_new[reference_overlap] <- ec_new[reference_overlap] * 1.8

plot(ec_new)

resistance_new<-log(1/ec_new )
plot(resistance_new)


r_coarse_new <- aggregate(resistance_new, fact = 2)

r_coarse_new[r_coarse_new <= 0] <- 0.01
r_coarse_new[is.infinite(values(r_coarse_new))] <- NA
plot(r_coarse_new)

# calculate moving window connectivity based on ecosystem condition in landscape
start<-Sys.time()
mw_result_new <- os_run(r_coarse_new, radius = 20, block_size = 10)
plot(mw_result_new$normalized_current)
print(Sys.time()-start)

writeRaster(mw_result_new$normalized_current,
            "output/ec_connect_new.tif",
            overwrite=TRUE)
plot(mw_result_new$normalized_current - mw_result_ini$normalized_current)
