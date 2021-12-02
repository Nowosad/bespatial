#' Configurational entropy for landscape mosaics
#'
#' @param x SpatRaster object ([terra::rast()]) containing one or more categorical rasters
#' @param nr_of_permutations Number of permutations performed on each input raster to calculate
#'   possible distribution of total edge values
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
#' mosaic = rast(system.file("raster/mosaic.tif", package = "bespatial"))
#' ce1 = conf_entropy1(mosaic, 1000)
#' plot(ce1$total_edge, ce1$lnp)
#' conf_entropy1(mosaic, 1000, independent = TRUE)
conf_entropy1 = function(x, nr_of_permutations, independent = FALSE){
  if (independent){
    result = lapply(terra::as.list(x), conf_entropy1,
                    nr_of_permutations, independent = FALSE)
    result = do.call(rbind, result)
  } else {
    p = permute_raster(x[[1]], nr_of_permutations)
    p_te = vapply(p, get_total_edge, resolution = terra::res(x), FUN.VALUE = numeric(1))
    mean_te = mean(p_te)
    sd_te = stats::sd(p_te)
    x_te = apply(terra::as.array(x), 3, get_total_edge, resolution = terra::res(x))
    lnp_te = stats::dnorm(x_te, mean = mean_te, sd = sd_te, log = TRUE)
    result = tibble::tibble(layer = seq_along(lnp_te),
                            metric = "configurational_entropy", type = "mosaic",
                            mean_total_edge = mean_te, sd_total_edge = sd_te,
                            lnp = lnp_te, total_edge = x_te)
  }
  return(result)
}
