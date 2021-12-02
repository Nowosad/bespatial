# set.seed(2021-10-11)
# ce3 = conf_entropy3b(point_pattern, 1000)
# 
# test_that("conf_entropy3 works", {
#   expect_equal(nrow(ce3), 11)
#   expect_equal(ncol(ce3), 7)
#   expect_equal(ce3$mean_neighbors[[1]], 40.1, tolerance = 0.001)
#   expect_equal(ce3$sd_neighbors[[1]], 8.14, tolerance = 0.001)
#   expect_equal(ce3$lnp[[1]], -3.016, tolerance = 0.001)
#   expect_equal(ce3$neighbors[[1]], 40, tolerance = 0.001)
# })
# 
# ce3b = conf_entropy3b(point_pattern, 1000, independent = TRUE)
# 
# test_that("conf_entropy3 works with independent=TRUE", {
#   expect_true(sd(ce3b$mean_neighbors) > 0)
# })
# 
