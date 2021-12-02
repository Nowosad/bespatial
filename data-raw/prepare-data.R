# examples prep - Fig.1 Cushman 2018 -------------------------------------
library(NLMR)
# devtools::install_github("ropensci/landscapetools")
library(landscapetools)
library(raster)
library(magrittr)
set.seed(2018-05-04)

random_map = nlm_random(ncol = 16, nrow = 16, resolution = 1, rescale = TRUE) %>%
  util_binarize(0.5)
random_map = setValues(raster(random_map), random_map[])
# plot(random_map)

# workworkwokr
h1_h10_map = seq(0.1, 1, 0.1) %>%
  purrr::map(~nlm_fbm(ncol = 16, nrow = 16, resolution = 1, fract_dim = ., user_seed = 2021-10-06)) %>%
  purrr::map(util_binarize, 0.5) %>%
  stack()
h1_h10_map = setValues(stack(h1_h10_map), h1_h10_map[])
# plot(h1_h10_map)

checker_values = rep(c(rep(c(1, 2), 8), rep(c(2, 1), 8)), 8)

checker_map = raster(ncol = 16, nrow = 16, resolution = 1,
                     xmn = 0, xmx = 16, ymn = 0, ymx = 16,
                     vals = checker_values, crs = NULL)

# plot(checker_map)

# combine the examples
fig1_maps = stack(random_map, h1_h10_map, checker_map)
names(fig1_maps) = c("random", paste0("H", sprintf("%02d", 1:10)), "checker")
plot(fig1_maps)

# usethis::use_data(fig1_maps, overwrite = TRUE)

mosaic = terra::rast(fig1_maps)
dir.create("inst/raster/", recursive = TRUE)
writeRaster(mosaic, "inst/raster/mosaic.tif", overwrite = TRUE)
