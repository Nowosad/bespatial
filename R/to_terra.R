to_terra = function(x) UseMethod("to_terra")
to_terra.RasterLayer = function(x) {
  x = terra::rast(x)
  return(x)
}
to_terra.RasterStack = function(x) {
  x = terra::rast(x)
  return(x)
}
to_terra.RasterBrick = function(x) {
  x = terra::rast(x)
  return(x)
}
to_terra.stars = function(x) {
  x = terra::rast(x)
  return(x)
}
to_terra.matrix = function(x) {
  x = terra::rast(x)
  return(x)
}
to_terra.array = function(x) {
  x = terra::rast(x)
  return(x)
}