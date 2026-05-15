# utils functions

## rescale a raster 0-1
rescale01 <- function(r) {

  rmin <- global(r, "min", na.rm=TRUE)[1,1]
  rmax <- global(r, "max", na.rm=TRUE)[1,1]

  (r - rmin) / (rmax - rmin)
}


# Function applied pixel-wise
euclidean_fun <- function(x) {

  # x = proportional ES values for one pixel
  if(any(is.na(x))) return(NA)

  sqrt(sum((w-x)^2))
}

#function to read newest tif from a folder
get_newest_tif <- function(folder, pattern = NULL) {

  # Base pattern for tif files
  tif_pattern <- "\\.tif$"

  # Add optional filename filter
  if (!is.null(pattern)) {
    tif_pattern <- paste0(pattern, ".*\\.tif$")
  }

  files <- list.files(
    folder,
    pattern = tif_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    stop("No matching .tif files found.")
  }

  newest_file <- sort(files, decreasing = TRUE)[1]

  return(newest_file)
}
