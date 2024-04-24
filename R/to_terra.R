to_terra = function(x) UseMethod("to_terra")
#' @export
to_terra.RasterLayer = function(x) {
  x = terra::rast(x)
  return(x)
}
#' @export
to_terra.RasterStack = function(x) {
  x = terra::rast(x)
  return(x)
}
#' @export
to_terra.RasterBrick = function(x) {
  x = terra::rast(x)
  return(x)
}
#' @export
to_terra.stars = function(x) {
  x = terra::rast(x)
  return(x)
}
#' @export
to_terra.matrix = function(x) {
  x = terra::rast(x)
  return(x)
}
#' @export
to_terra.array = function(x) {
  x = terra::rast(x)
  return(x)
}