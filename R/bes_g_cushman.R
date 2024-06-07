#' Configurational entropy for surfaces
#' 
#' Calculates Cushman's configurational entropy for surfaces (2021)
#'
#' @param x SpatRaster, stars, RasterLayer, RasterStack, RasterBrick, matrix, or array containing one or more continuous rasters
#' @param nr_of_permutations  Number of permutations performed on each input raster to calculate
#'   possible distribution of "slope" values
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
#' \donttest{
#'   library(bespatial)
#'   library(terra)
#'   gradient = rast(system.file("raster/gradient.tif", package = "bespatial"),
#'                        lyrs = 1)
#'   ce2 = bes_g_cushman(gradient, 100)
#'   plot(gradient, main = round(ce2$value, 2))
#'   bes_g_cushman(gradient, 1000, independent = TRUE)
#' }
bes_g_cushman = function(x, nr_of_permutations = 1000, independent = FALSE){
  if (!inherits(x, "SpatRaster")){
    x = to_terra(x)
  }
  if (independent){
    result = lapply(terra::as.list(x), bes_g_cushman,
                    nr_of_permutations, independent = FALSE)
    result = do.call(rbind, result)
  } else {
    p = permute_raster(x[[1]], nr_of_permutations)
    p_slope = vapply(p, get_slope, FUN.VALUE = numeric(1))
    mean_slope = mean(p_slope)
    sd_slope = stats::sd(p_slope)
    x_slope = apply(terra::as.array(x), 3, get_slope)
    lnp_slope = stats::dnorm(x_slope, mean = mean_slope, sd = sd_slope, log = TRUE)
    result = tibble::tibble(layer = seq_along(lnp_slope),
                            type = "gradient",
                            metric = "cushman",
                            value = lnp_slope)
  }
  return(result)
}
