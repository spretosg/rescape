library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(sf)
library(tools)

raster_dir <- "es"

raster_files <- list.files(
  raster_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Read rasters
rasters <- lapply(raster_files, rast)

# Use file names as layer names
names(rasters) <- file_path_sans_ext(basename(raster_files))

# =========================================================
# CREATE COLOR PALETTES AUTOMATICALLY
# =========================================================

palette_names <- c(
  "viridis",
  "plasma",
  "magma",
  "inferno",
  "cividis"
)

palettes <- lapply(seq_along(rasters), function(i) {

  colorNumeric(
    palette = palette_names[(i - 1) %% length(palette_names) + 1],
    domain = values(rasters[[i]]),
    na.color = "transparent"
  )

})

names(palettes) <- names(rasters)

ui <- page_fillable(

  tags$head(
    h2(
      "ReSCAPE mapviewer økosystemtjenester",
    ),
    tags$style(HTML("

      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        overflow: hidden;
      }

      .leaflet-container {
        background: #f8f9fa;
      }

      #controls {
        background: rgba(255,255,255,0.95);
        padding: 15px;
        border-radius: 12px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
      }

    "))
  ),

  div(
    style = "
      position: relative;
      width: 100vw;
      height: 100vh;
    ",

    # Fullscreen map
    leafletOutput(
      "map",
      width = "100%",
      height = "100%"
    ),

    # Floating control panel
    absolutePanel(
      id = "controls",
      top = 20,
      left = 20,
      width = 320,
      draggable = TRUE,

      h4("Raster Controls"),

      selectInput(
        inputId = "raster_select",
        label = "Raster layer",
        choices = names(rasters),
        selected = names(rasters)[1]
      ),

      sliderInput(
        inputId = "threshold",
        label = "Minimum value",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01
      )
    )
  )
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({

    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = TRUE
      )
    ) %>%

      # addTiles(
      #   urlTemplate = "https://cache.kartverket.no/v1/wmts/1.0.0/topograatone/default/webmercator/{z}/{y}/{x}.png",
      #   attribution = "© Kartverket",
      #   group = "Kartverket Topo Gråtone"
      # ) %>%

      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite"
      ) %>%



      addLayersControl(
        baseGroups = c(
          "Kartverket Topo Gråtone"
          # "Satellite"
        ),
        overlayGroups = c("Raster"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%

      setView(lng = 10.75, lat = 63.4, zoom = 12)

  })

  filtered_raster <- reactive({

    r <- rasters[[input$raster_select]]

    r_filtered <- r

    values(r_filtered)[values(r_filtered) < input$threshold] <- NA

    r_filtered

  })

  observe({

    req(filtered_raster())

    proxy <- leafletProxy("map")

    proxy %>%
      clearGroup("Raster") %>%
      clearControls()

    proxy %>%

      addRasterImage(
        filtered_raster(),
        colors = palettes[[input$raster_select]],
        opacity = 0.8,
        project = TRUE,
        group = "Raster"
      ) %>%

      addLegend(
        pal = palettes[[input$raster_select]],
        values = values(rasters[[input$raster_select]]),
        title = input$raster_select,
        position = "bottomright"
      ) %>%

      showGroup("Raster")

  })

}

shinyApp(ui, server)
