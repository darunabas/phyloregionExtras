#' Extracts environmental variables
#'
#' This function extracts environmental variables from raster
#' files within spatial spatial polygons.
#'
#' @param x A raster file of the environmental variable
#' @param y Spatial polygons data frame corresponding to
#' the study area of interest to extract the variable.
#' @param val String to rename the column with the extracted
#' variable.
#' @param FUN The function to summarize the values (e.g. sd, mean)
#' @param \dots arguments passed among methods.
#'
#' @rdname extract_climate
#' @importFrom raster projectRaster extract
#' @importFrom sp proj4string
#' @importFrom stats sd
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @return A list or data frame.
#'
#' @author Barnabas H. Daru \email{darunabas@gmail.com}
#'
#'
#' @export
extract_climate <- function(x, y, val = "val", FUN = sd, ...){
  x <- raster::projectRaster(x, crs = ('+proj=longlat'))
  ras <- raster::projectRaster(x, crs = proj4string(y), over = TRUE)
  p <- raster::extract(ras, y, ...)
  pb <- txtProgressBar(min = 0, max = length(p), style = 3)
  m <- unlist(lapply(p, function(x) {
    if (!is.null(x)) FUN(x, na.rm = TRUE) else NA
    setTxtProgressBar(pb, x)
  }))
  y$foo <- m
  names(y)[match("foo", names(y))] <- val
  y
}
