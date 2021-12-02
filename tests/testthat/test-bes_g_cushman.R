set.seed(2021-10-11)
ce2 = bes_g_cushman(gradient, 1000)

test_that("bes_g_cushman works", {
  expect_equal(nrow(ce2), 11)
  expect_equal(ncol(ce2), 4)
  # expect_equal(ce2$mean_slope[[1]], 12.8, tolerance = 0.001)
  # expect_equal(ce2$sd_slope[[1]], 0.181, tolerance = 0.001)
  expect_equal(ce2$value[[1]], 0.482, tolerance = 0.001)
  # expect_equal(ce2$slope[[1]], 12.66, tolerance = 0.001)
})

# ce2b = bes_g_cushman(gradient, 1000, independent = TRUE)
# 
# test_that("bes_g_cushman works with independent=TRUE", {
#   expect_true(sd(ce2b$mean_slope) > 0)
# })

