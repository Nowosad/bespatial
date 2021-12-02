#' Boltzmann entropy of a landscape gradient
#'
#' Calculates the Boltzmann entropy of a landscape gradient
#'
#' @param x SpatRaster, stars, RasterLayer, RasterStack, RasterBrick, matrix, or array.
#' @param base A logarithm base ("log", "log2" or "log10").
#' @param relative Should a relative or absolute entropy be calculated? TRUE or FALSE (default).
#' @param method A method used. Either "hierarchy" for
#' the hierarchy-based method (Gao et al., 2017) or "aggregation" (default)
#' for the aggregation-based method (Gao et al., 2019).
#' @param na_adjust Should the output value be adjusted to the proportion of not missing cells? Either TRUE (default) or FALSE
#'
#' @details The method for computing the Boltzmann entropy of a landscape
#' gradient works on integer values that are either positive or equals to zero.
#' This function automatically rounds values to the nearest integer value
#' (rounding halfway cases away from zero) and negative values are shifted to
#' positive values.
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "A hierarchy-based
#' solution to calculate the configurational entropy of landscape gradients."
#' Landscape Ecology 32.6 (2017): 1133-1146.
#'
#' @references Gao, Peichao, Hong Zhang, and Zhilin Li. "An efficient
#' analytical method for computing the Boltzmann entropy of a landscape
#' gradient." Transactions in GIS (2018).
#'
#' @references Gao, Peichao and Zhilin Li. "Aggregation-based method
#' for computing absolute Boltzmann entropy of landscape gradient
#' with full thermodynamic consistency"
#' Landscape Ecology (2019)
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(terra)
#' library(bespatial)
#' gradient = rast(system.file("raster/gradient.tif", package = "bespatial"))
#' gg1 = bes_g_gao(gradient)
#' plot(gradient, main = round(gg1$value, 2))
bes_g_gao = function(x,
                         method = "aggregation",
                         na_adjust = TRUE,
                         base = "log10",
                         relative = FALSE){
  value = get_boltzmann(x = x, method = method, na_adjust = na_adjust,
                        base = base, relative = relative)
  result = tibble::tibble(layer = seq_along(value),
                          type = "gradient",
                          metric = "gao", 
                          value = value)
  return(result)
}
