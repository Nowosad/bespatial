#' Configurational entropy for landscape mosaics
#' 
#' Calculates Cushman's configurational entropy for landscape mosaics (2015)
#'
#' @param x SpatRaster, stars, RasterLayer, RasterStack, RasterBrick, matrix, or array containing one or more categorical rasters
#' @param nr_of_permutations Number of permutations performed on each input raster to calculate
#'   possible distribution of total edge values
#' @param independent Should an independent set of permutations be performed for each input raster?
#'   `TRUE`/`FALSE`.
#'   Use `FALSE` (default) when each of your input rasters has the same configuration (proportion of categories).
#'
#' @return A tibble
#' @export
#'
#' @references Cushman, S. A. (2015). Calculating the configurational entropy of a landscape mosaic. In Landscape Ecology (Vol. 31, Issue 3, pp. 481–489). Springer Science and Business Media LLC. https://doi.org/10.1007/s10980-015-0305-2 
#' @examples
#' library(terra)
#' library(bespatial)
#' mosaic = rast(system.file("raster/mosaic.tif", package = "bespatial"))
#' ce1 = bes_m_cushman(mosaic, 1000)
#' plot(mosaic, main = round(ce1$value, 2))
#' bes_m_cushman(mosaic, 1000, independent = TRUE)
bes_m_cushman = function(x, nr_of_permutations, independent = FALSE){
  if (!inherits(x, "SpatRaster")){
    x = to_terra(x)
  }
  if (independent){
    result = lapply(terra::as.list(x), bes_m_cushman,
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
                            type = "mosaic",
                            metric = "cushman",
                            value = lnp_te)
  }
  return(result)
}
