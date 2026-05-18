library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(sf)

# Load rasters
ndvi_prio <- rast("rest_prio_ndvi_20260515_122123.tif")
grey_prio <- rast("rest_prio_grey_20260515_122123.tif")
nor_cons_no_grey <- sf::st_read("norcons_no_grey_diss.gpkg")

# Raster list
rasters <- list(
  "NDVI Priority" = ndvi_prio,
  "Grey Priority" = grey_prio
)

# Color palettes
palettes <- list(
  "NDVI Priority" = colorNumeric(
    "viridis",
    domain = values(ndvi_prio),
    na.color = "transparent"
  ),

  "Grey Priority" = colorNumeric(
    "plasma",
    domain = values(grey_prio),
    na.color = "transparent"
  )
)

ui <- page_fillable(

  tags$head(
    h2(
      "ReSCAPE mapviewer",
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
        selected = "NDVI Priority"
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
      addPolygons(
        data = nor_cons_no_grey,
        group = "Norconsult restoration no grey",
        weight = 2,
        color = "red",
        fillOpacity = 0.2
      ) %>%


      addLayersControl(
        baseGroups = c(
          "Kartverket Topo Gråtone"
          # "Satellite"
        ),
        overlayGroups = c("Raster","Norconsult restoration no grey"),
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
