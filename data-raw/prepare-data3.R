# examples prep - Fig.3 Cushman 2021 -------------------------------------
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
fig3_maps = stack(random_map, h1_h10_map)
names(fig3_maps) = c("random", paste0("H", sprintf("%02d", 1:10)))
fig3_maps[fig3_maps<10] = NA
plot(fig3_maps)

# library(terra)
# library(sf)
# library(tidyr)
# library(dplyr)
# fig3_maps = st_as_sf(as.polygons(rast(fig3_maps), dissolve = FALSE))
# fig3_maps = pivot_longer(fig3_maps, cols = c("random", paste0("H", sprintf("%02d", 1:10))))
# fig3_maps = filter(fig3_maps, value == 10)
# fig3_maps = st_centroid(fig3_maps)
#
# library(tmap)
# tm_shape(fig3_maps) +
#   tm_dots() +
#   tm_facets("name", free.coords = FALSE)

# usethis::use_data(fig3_maps, overwrite = TRUE)

point_pattern = terra::rast(fig3_maps)
writeRaster(point_pattern, "inst/raster/point_pattern.tif", overwrite = TRUE)

# to vector
library(sf)
library(terra)
library(tidyr)
point_pattern = rast("inst/raster/point_pattern.tif")

pp_list1 = as.list(point_pattern)
pp_list2 = lapply(pp_list1, as.points)
pp_list3 = lapply(pp_list2, st_as_sf)

f1 = function(x){
  x$type = names(x)[1]
  x[[1]] = NULL
  x
}
pp_list4 = lapply(pp_list3, f1)
pp_list5 = do.call(rbind, pp_list4)

dir.create("inst/vector")
write_sf(pp_list5, "inst/vector/point_pattern.gpkg")
