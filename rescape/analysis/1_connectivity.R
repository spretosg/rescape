library(circuitscaper)
library(terra)
source("analysis/utils.R")
#cs_install_julia()
Sys.which("julia")
system("julia --version")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


#### ---- input data ----
ec<-terra::rast("data/ec.tif")
ec<-rescale01(ec)

lulc<-terra::rast("data/lulc.tif")
lulc[lulc == 0] <- NA
lulc <- trunc(lulc / 100)

#### ---- resistance grid ----
lulc<-project(lulc,crs(ec))
# ensure same geometry
lulc <- resample(lulc, ec, method = "near")

## for each lulc type attach a factor multiplying the EC
## higher values == easier to move through area
factors <- data.frame(
  class = c(1, 2, 3, 4, 5),
  factor = c(0.1, 1, 3, 4,4)
)

factor_raster <- classify(
  lulc,
  rcl = as.matrix(factors)
)

# --- Multiply ec raster by factors ---
r_adjusted <- ec * factor_raster


# resistance_ini<-1/log(r_adjusted )
resistance_ini <- 1/r_adjusted
plot(resistance_ini)

r_coarse_ini <- aggregate(resistance_ini, fact = 2)

r_coarse_ini[r_coarse_ini <= 0] <- 0.01
r_coarse_ini[is.infinite(values(r_coarse_ini))] <- NA
plot(r_coarse_ini)

#### ---- circuitscape connectivity ----
# calculate moving window connectivity based on ecosystem condition in landscape
start<-Sys.time()
mw_result_ini <- os_run(r_coarse_ini, radius = 30, block_size = 5)
plot(mw_result_ini$normalized_current)
print(Sys.time()-start)

file_name <- paste0("output/connectivity/ec_connectivity_", timestamp, ".tif")

names(mw_result_ini$normalized_current) <- "connectivity"
writeRaster(mw_result_ini$normalized_current,
            file_name,
            overwrite=TRUE)



