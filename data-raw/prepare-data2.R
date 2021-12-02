# examples prep - Fig.2 Cushman 2021 -------------------------------------
library(NLMR)
# devtools::install_github("ropensci/landscapetools")
library(landscapetools)
library(raster)
library(magrittr)
set.seed(2021-12-07)

random_map = nlm_random(ncol = 32, nrow = 32, resolution = 1, rescale = TRUE) %>%
  landscapetools::util_classify(weighting = rep(0.1, 10))
random_map = setValues(raster(random_map), random_map[])

# plot(random_map)

# workworkwokr
h1_h10_map = seq(0.1, 1, 0.1) %>%
  purrr::map(~nlm_fbm(ncol = 32, nrow = 32, resolution = 1, fract_dim = ., user_seed = 2021-12-07)) %>%
  purrr::map(landscapetools::util_classify, weighting = rep(0.1, 10)) %>%
  raster::stack()
h1_h10_map = setValues(stack(h1_h10_map), h1_h10_map[])

# plot(h1_h10_map)

# combine the examples
fig2_maps = stack(random_map, h1_h10_map)
names(fig2_maps) = c("random", paste0("H", sprintf("%02d", 1:10)))
plot(fig2_maps)

# usethis::use_data(fig2_maps, overwrite = TRUE)

surface = terra::rast(fig2_maps)
writeRaster(surface, "inst/raster/surface.tif", overwrite = TRUE)
