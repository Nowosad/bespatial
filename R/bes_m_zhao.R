#' Zhao's entropy for landscape mosaics
#' 
#' Calculates Zhao's entropy for landscape mosaics based on the Wasserstein metric (2019) 
#'
#' @param x SpatRaster, stars, RasterLayer, RasterStack, RasterBrick, matrix, or array containing one or more categorical rasters
#' @param neighbourhood The number of directions in which cell adjacencies are
#' considered as neighbours: 4 (rook's case), 8 (queen's case) 
#'
#' @return A tibble
#' @export
#'
#' @references Zhao, Y., & Zhang, X. (2019). Calculating spatial configurational entropy of a landscape mosaic based on the Wasserstein metric. Landscape Ecology, 34(8), 1849-1858. https://doi.org/10.1007/s10980-019-00876-x
#' @examples
#' library(terra)
#' library(bespatial)
#' mosaic = rast(system.file("raster/mosaic.tif", package = "bespatial"))
#' w_dists1 = bes_m_zhao(mosaic, 1000)
#' plot(mosaic, main = round(w_dists1$value, 2))
bes_m_zhao = function(x, neighbourhood = 4){
  if (!inherits(x, "SpatRaster")){
    x = to_terra(x)
  }
  w_dists = vapply(x, bes_m_zhao_single, FUN.VALUE = numeric(1), directions = neighbourhood)
  result = tibble::tibble(layer = seq_along(w_dists), 
                          type = "mosaic",
                          metric = "zhao",
                          value = w_dists)
  return(result)
}

bes_m_zhao_single = function(x, directions){
  # composition
  h_data = as.data.frame(freq(x))
  h_data$freq = h_data$count/sum(h_data$count)
  el1 = create_expanded_log(h_data$count)
  wc1 = wasserstein_metric(el1)
  ncell = sum(el1$w)
  yc = data.frame(v = 1:ncell, p = 1/ncell)
  wc2 = wasserstein_metric(yc)
  # configuration
  rev_scale = (prod(res(x)) / 10000)
  areas = landscapemetrics::lsm_p_area(x, directions = directions)
  areas$value_ncells = areas$value / rev_scale
  areas$freq = areas$value_ncells/sum(areas$value_ncells)
  el2 = create_expanded_log(areas$value_ncells)
    # y = patches(x)
  # z = cellSize(y,unit="ha") |> zonal(y, sum)
  # x_areas = terra::expanse(terra::disagg(terra::as.polygons(mosaic)), unit = "ha", transform = FALSE)
  # x_areas = x_areas/sum(x_areas)
  # x_areas / rev_scale
  el2 = create_expanded_log(areas$value_ncells)
  ws1 = wasserstein_metric(el2)
  ncell = sum(el2$w)
  ys = data.frame(v = 1:ncell, p = 1/ncell)
  ws2 = wasserstein_metric(ys)
  # final value
  wc = wc1/wc2
  ws = ws1/ws2
  w_dist = (1 - (wc)) * (1 - (ws))
  w_dist
}

create_expanded_log = function(counts){
  sizes = vector(mode = "integer", length = max(counts))
  for (i in seq_len(max(counts))){
    sizes[i] = length(counts[counts >= i])
  }
  data.frame(v = seq_along(sizes),
             w = sizes,
             p = sizes/sum(sizes)
  )
}
wasserstein_metric = function(df){
  sum((df$p * log(df$v)))
}