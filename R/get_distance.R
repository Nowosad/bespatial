#' Calculates an average distance between non-NA cells
#'
#' @param p A matrix
#' @param x A SpatRaster with proper metadata (e.g., extent and CRS)
#'
#' @details It converts permuted matrix into a vector dataset, and
#' calculates an average distance between the points
#'
#' @return An average distance between points
get_distance = function(p, x){
  new_r = terra::as.points(terra::rast(x, vals = p, nlyr = 1))
  mean(as.vector(terra::distance(new_r, method="geo")))
}
