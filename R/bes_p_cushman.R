#' Configurational entropy for point patterns
#' 
#' Calculates Cushman's configurational entropy for point patterns (2021)
#'
#' @param x SpatRaster, stars, RasterLayer, RasterStack, RasterBrick, matrix, or array containing one or more rasters with one value and NAs
#' @param nr_of_permutations Number of permutations performed on each input raster to calculate
#'   possible distribution of the number of nearest neighbors
#' @param independent Should an independent set of permutations be performed for each input raster?
#'   `TRUE`/`FALSE`.
#'   Use `FALSE` (default) when each of your input rasters has the same configuration.
#'
#' @return A tibble
#' @export
#' 
#' @references Cushman, S. A. (2021). Generalizing Boltzmann Configurational Entropy to Surfaces, Point Patterns and Landscape Mosaics. In Entropy (Vol. 23, Issue 12, p. 1616). MDPI AG. https://doi.org/10.3390/e23121616 
#'
#' @examples
#' library(terra)
#' library(bespatial)
#' point_pattern = rast(system.file("raster/point_pattern.tif", package = "bespatial"))
#' ce3 = bes_p_cushman(point_pattern, 100)
#' plot(point_pattern, main = round(ce3$value, 2))
#' ce3b = bes_p_cushman(point_pattern, 100, independent = TRUE)
#' plot(point_pattern, main = round(ce3b$value, 2))
bes_p_cushman = function(x, nr_of_permutations, independent = FALSE){
  if (!inherits(x, "SpatRaster")){
    x = to_terra(x)
  }
  if (independent){
    result = lapply(terra::as.list(x), bes_p_cushman,
                    nr_of_permutations, independent = FALSE)
    result = do.call(rbind, result)
  } else {
    p = permute_raster(x[[1]], nr_of_permutations)
    p_dist = vapply(p, get_distance, x, FUN.VALUE = numeric(1))
    mean_dist = mean(p_dist)
    sd_dist = stats::sd(p_dist)
    x_dist = apply(terra::as.array(x), 3, get_distance, x)
    lnp_dist = stats::dnorm(x_dist, mean = mean_dist, sd = sd_dist, log = TRUE)
    result = tibble::tibble(layer = seq_along(lnp_dist),
                            type = "point_pattern",
                            metric = "cushman", 
                            value = lnp_dist)
  }
  return(result)
}

