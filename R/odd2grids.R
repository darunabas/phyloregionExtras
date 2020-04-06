#' Convert irregular community data to grids
#'
#' This functions converts irregular polygon shapefiles to grids
#' @param x  input data framme.
#' @param res the grain size of the grid cells in decimal degrees (default).
#' @param shp the irregular polygon shapefile to be converted
#' @rdname odd2grids
#' @importFrom data.table rbindlist
#' @importFrom sp over
#' @importFrom stats complete.cases
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return
#' \itemize{
#'   \item comm_dat: community data frame
#'   \item poly_shp: shapefile of grid cells with the values per cell.
#' }
#' @export
odd2grids <- function(x, shp, res = 0.25){
  f1 <- phyloregion::fishnet(shp, res = res)
  ss <- cbind(as.data.frame(f1), over(f1, shp))
  names(ss)[1] <- "new_grids"
  df <- split(x , f = x$species)
  pb <- txtProgressBar(min = 0, max = length(df), style = 3)
  spo <- lapply(df, function(x) {
    index <- match(ss$grids, x$grids)
    m <- cbind(ss, species=x$species[index])
    m <- m[complete.cases(m),]
    m <- m[, c("new_grids", "species")]
    setTxtProgressBar(pb, x)
  })
  r <- data.table::rbindlist(spo)
  res <- data.frame(table(r$new_grids))
  names(res) <- c("grids", "sr")
  z <- merge(f1, res, by="grids")
  z <- z[!is.na(z@data$sr),]
  return(list(comm_dat=r, poly_shp=z))
}
