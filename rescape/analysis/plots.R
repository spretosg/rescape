#plots
library(sf)
library(terra)
library(ggplot2)

stud_area<-st_read("data/stud_site.gpkg")
# convert sf -> terra vector
boundary_vect <- vect(stud_area)




rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}

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
ggplot() +
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
    colours = c("red", "yellow", "green"),
    limits = c(0, 1)
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL
  )

con<-terra::rast("output/ec_connect_ini4.tif")
con<-project(con,crs(stud_area))
r_df <- as.data.frame(con, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "connectivity"


ggplot() +
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




r_df <- as.data.frame(mw_result_ini$normalized_current, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "connectivity"


ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = connectivity)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_gradientn(
    colours = c(
      "#8b0000",   # dark red
      "#8b0000",   # red
      "#ffd966",   # yellow near 1
      "#90ee90",   # light green
      "#006400"    # dark green
    ),
    values = rescale(c(0, 0.99, 1, 2, 4)),
    limits = c(0, 4),
    oob = squish,
    name = "Connectivity"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )


## Øs
cult<-terra::rast("output/es/cult_mean.tif")
cult <- crop(cult, boundary_vect)

# remove cells outside polygon
cult <- mask(cult, boundary_vect)

plot(cult)
r_df <- as.data.frame(cult, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "cult_es"


ggplot() +
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


reg<-terra::rast("output/es/reg_mean.tif")
reg <- crop(reg, boundary_vect)

# remove cells outside polygon
reg <- mask(reg, boundary_vect)

plot(reg)
r_df <- as.data.frame(reg, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "reg_es"


ggplot() +
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

prov<-terra::rast("output/es/reg_mean.tif")
prov <- crop(prov, boundary_vect)

# remove cells outside polygon
prov <- mask(prov, boundary_vect)


r_df <- as.data.frame(prov, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "prov"


ggplot() +
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


wssi<-terra::rast("output/wssi.tif")
wssi <- crop(wssi, boundary_vect)

# remove cells outside polygon
wssi <- mask(wssi, boundary_vect)
wssi<-rescale01(wssi)

r_df <- as.data.frame(wssi, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "wssi"



ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = wssi)
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


rest_pot<-rast("output/REST_POT_NEW.tif")
rest_pot[rest_pot <=0] <- NA
q90<-quantile(values(rest_pot),0.9, na.rm=T)
q10<-quantile(values(rest_pot),0.1, na.rm=T)



r_df <- as.data.frame(rest_pot, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rest_pot"


ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rest_pot)
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_gradientn(
    colours = c(
      "#8b0000",   # dark red
      "#ffd966",   # yellow near 1
      "#ffd966",   # light green
      "#006400"    # dark green
    ),
    values = rescale(c(0, 0.01, q90, 1)),
    limits = c(0, 1),
    oob = squish,
    name = "Connectivity"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )


wssi<-terra::rast("output/wssi.tif")
wssi <- crop(wssi, boundary_vect)

# remove cells outside polygon
wssi <- mask(wssi, boundary_vect)
wssi<-rescale01(wssi)

r_df <- as.data.frame(wssi, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "wssi"



ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = wssi)
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

# effect
rest_effect<-rast("output/rest_effect.tif")
r_df <- as.data.frame(rest_effect, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rest_effect"



ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rest_effect)
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
    name = "rest_effect"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

grey<-rast("output/sum_grey.tif")

r_df <- as.data.frame(grey, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rel_grey"



ggplot() +
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


trend<-terra::rast("data/es_trend_dat.tif")
# NDVI_trend<-trend[[22]]
NDVI_trend<-trend[[4]]
NDVI_trend <- crop(NDVI_trend, boundary_vect)

# remove cells outside polygon
NDVI_trend <- mask(NDVI_trend, boundary_vect)
NDVI_trend<-as.factor(NDVI_trend)
plot(NDVI_trend)
NDVI_trend[NDVI_trend == -32768] <- NA

r_df <- as.data.frame(NDVI_trend, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "trend"



ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = factor(trend)),
  ) +
  geom_sf(
    data = stud_area,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "-3" = "#b2182b",  # dark red
      "-2" = "#ef8a62",  # red
      "-1" = "#fddbc7",  # light red
      "0"  = NA,  # orange
      "1"  = "#d9f0d3",  # light green
      "2"  = "#78c679",  # green
      "3"  = "#006837"   # dark green
    ),
    name = "trend"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

rest_grey<-rast("output/rest_pot_grey.tif")
rest_trend<-rast("output/rest_pot_trend.tif")

r_df <- as.data.frame(rest_trend, xy = TRUE, na.rm = TRUE)

# Rename raster value column
colnames(r_df)[3] <- "rest_trend"


ggplot() +
  geom_raster(
    data = r_df,
    aes(x = x, y = y, fill = rest_trend)
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
