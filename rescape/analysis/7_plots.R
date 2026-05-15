#plots
library(sf)
library(terra)
library(ggplot2)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
source("analysis/utils.R")

stud_area<-st_read("data/stud_site.gpkg")
# convert sf -> terra vector
boundary_vect <- vect(stud_area)


####### ecosystem condition
ec_status<-terra::rast("data/ec.tif")
ec_status<-rescale01(ec_status)
# crop first (faster)
ec_status <- crop(ec_status, boundary_vect)

# remove cells outside polygon
ec_status <- mask(ec_status, boundary_vect)

# --- Convert raster to dataframe for ggplot ---
r_df <- as.data.frame(ec_status, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "ecosystem_condition"

# --- Plot ---
ec_plot<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = ecosystem_condition)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  scale_fill_gradientn(
    colours = c("orange", "yellow", "green"),
    limits = c(0, 1)
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL
  )
ggsave(paste0("output/plots/ec_",timestamp,".png"), plot = ec_plot, width = 8, height = 6, dpi = 300)


con <- get_newest_tif("output/connectivity")
con<-terra::rast(con)
con<-project(con,crs(stud_area))

r_df <- as.data.frame(con, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "connectivity"


con<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = connectivity)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c("red", NA, "lightgreen", "lightgreen", "darkgreen"),
    values = scales::rescale(c(0, 0.9, 2, 3, 4)),
    limits = c(0, 4),
    oob = scales::squish
  ) +
  coord_sf() +
  theme_minimal()
ggsave(paste0("output/plots/con_ec_",timestamp,".png"), plot = ec_plot, width = 8, height = 6, dpi = 300)

## Øs
cult<-terra::rast("output/es/cult_mean.tif")
cult <- crop(cult, boundary_vect)

# remove cells outside polygon
cult <- mask(cult, boundary_vect)
r_df <- as.data.frame(cult, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "cult_es"


cult<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = cult_es)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +

  coord_sf() +
  theme_minimal()
ggsave(paste0("output/plots/es_cult_",timestamp,".png"), plot = cult, width = 8, height = 6, dpi = 300)


reg<-terra::rast("output/es/reg_mean.tif")
reg <- crop(reg, boundary_vect)

# remove cells outside polygon
reg <- mask(reg, boundary_vect)

r_df <- as.data.frame(reg, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "reg_es"


reg<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = reg_es)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#fff5eb",  # very light orange
      "#fdd0a2",
      "#fdae6b",
      "#f16913",
      "#a63603"   # dark orange/brown
    ),
    name = "reg_es"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/es_reg_",timestamp,".png"), plot = reg, width = 8, height = 6, dpi = 300)


prov<-terra::rast("output/es/reg_mean.tif")
prov <- crop(prov, boundary_vect)

# remove cells outside polygon
prov <- mask(prov, boundary_vect)


r_df <- as.data.frame(prov, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "prov"


prov<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = prov)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#f7fcf5",  # very light green
      "#c7e9c0",
      "#74c476",
      "#31a354",
      "#00441b"   # dark green
    ),
    name = "prov"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/es_prov_",timestamp,".png"), plot = prov, width = 8, height = 6, dpi = 300)



multi <- get_newest_tif("output/wssi","wssi")
multi<-terra::rast(multi)
# remove cells outside polygon
multi <- mask(multi, boundary_vect)
multi<-rescale01(multi)

r_df <- as.data.frame(multi, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "multifunctionality"



multi<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = multifunctionality)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#f6efe7",  # very light beige
      "#d9b38c",
      "#b07d52",
      "#8c5a2b",
      "#4b2e14"   # dark brown
    ),
    name = "wssi"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/wssi_",timestamp,".png"), plot = multi, width = 8, height = 6, dpi = 300)

## eco deficit

eco_def <- get_newest_tif("output/eco_deficit")
eco_def<-terra::rast(eco_def)
eco_def<-log(eco_def)
r_df <- as.data.frame(eco_def, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "eco_deficit"



def<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = eco_deficit)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#313695",  # deep blue (cold)
      "#74add1",  # light blue
      "#ffffbf",  # neutral yellow
      "#f46d43",  # orange
      "#a50026"   # deep red (hot)
    ),
    name = "Log(Eco_deficit)"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/eco_def_",timestamp,".png"), plot = def, width = 8, height = 6, dpi = 300)

grey<-rast("data/sum_grey_20260515_110317.tif")

r_df <- as.data.frame(grey, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rel_grey"

grey<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rel_grey)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#f7f7f7",  # very light grey
      "#d9d9d9",
      "#bdbdbd",
      "#737373",
      "#252525"   # near black
    ),
    name = "relative grey"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/rel_grey_",timestamp,".png"), plot = grey, width = 8, height = 6, dpi = 300)



trend<-terra::rast("data/perform_trend_140526.tif")

r_df <- as.data.frame(trend, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "trend"



trend<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = trend),
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c("#b2182b","#ef8a62","#fddbc7", NA, "#d9f0d3", "#78c679", "darkgreen"),
    values = scales::rescale(c(-100, -50,-25, 0, 25, 50,100)),
    limits = c(-100, 100),
    oob = scales::squish
  )+
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/ndv_trend_",timestamp,".png"), plot = trend, width = 8, height = 6, dpi = 300)


rest_grey <- get_newest_tif("output/rest_prio","grey")
rest_grey<-terra::rast(rest_grey)

r_df <- as.data.frame(rest_grey, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rest_grey"


rest_grey<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rest_grey)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#fff5eb",  # very light orange
      "#fdd0a2",
      "#fdae6b",
      "#f16913",
      "#a63603"   # dark orange/brown
    ),
    name = "Restoration prio. grey areas"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/rest_prio_grey_",timestamp,".png"), plot = rest_grey, width = 8, height = 6, dpi = 300)



rest_trend <- get_newest_tif("output/rest_prio","ndvi")
rest_trend<-terra::rast(rest_trend)

r_df <- as.data.frame(rest_trend, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rest_ndvi"


rest_ndvi<-ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rest_ndvi)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#fff5eb",  # very light orange
      "#fdd0a2",
      "#fdae6b",
      "#f16913",
      "#a63603"   # dark orange/brown
    ),
    name = "Restoration prio. NDVI trend"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
ggsave(paste0("output/plots/rest_prio_ndvi_",timestamp,".png"), plot = rest_ndvi, width = 8, height = 6, dpi = 300)
