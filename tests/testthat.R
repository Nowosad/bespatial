library(testthat)
library(bespatial)
library(terra)
mosaic = rast(system.file("raster/mosaic.tif", package = "bespatial"))
gradient = rast(system.file("raster/gradient.tif", package = "bespatial"))
point_pattern = rast(system.file("raster/point_pattern.tif", package = "bespatial"))

test_check("bespatial")
