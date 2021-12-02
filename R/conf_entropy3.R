#' Configurational entropy for point patterns
#'
#' @param x SpatRaster object ([terra::rast()]) containing one or more rasters with one value and NAs
#' @param nr_of_permutations Number of permutations performed on each input raster to calculate
#'   possible distribution of the number of nearest neighbors
#' @param independent Should an independent set of permutations be performed for each input raster?
#'   `TRUE`/`FALSE`.
#'   Use `FALSE` (default) when each of your input rasters has the same configuration.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(terra)
#' library(bespatial)
#' point_pattern = rast(system.file("raster/point_pattern.tif", package = "bespatial"))
#' ce3 = conf_entropy3(point_pattern, 100)
#' plot(ce3$distance, ce3$lnp)
#' ce3b = conf_entropy3(point_pattern, 100, independent = TRUE)
#' plot(ce3b$distance, ce3b$lnp)
conf_entropy3 = function(x, nr_of_permutations, independent = FALSE){
  if (independent){
    result = lapply(terra::as.list(x), conf_entropy3,
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
                            metric = "configurational_entropy", type = "point_pattern",
                            mean_distance = mean_dist, sd_distance = sd_dist,
                            lnp = lnp_dist, distance = x_dist)
  }
  return(result)
}

