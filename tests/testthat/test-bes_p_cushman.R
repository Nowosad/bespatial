set.seed(2021-10-11)
ce3 = bes_p_cushman(point_pattern, 100)

test_that("bes_p_cushman works", {
  expect_equal(nrow(ce3), 11)
  expect_equal(ncol(ce3), 4)
  # expect_equal(ce3$mean_distance[[1]], 1806823, tolerance = 0.001)
  # expect_equal(ce3$sd_distance[[1]], 51536, tolerance = 0.001)
  expect_equal(ce3$value[[1]], -12.6, tolerance = 0.001)
  # expect_equal(ce3$distance[[1]], 1873657, tolerance = 0.001)
})

# ce3b = bes_p_cushman(point_pattern, 100, independent = TRUE)
# 
# test_that("bes_p_cushman works with independent=TRUE", {
#   expect_true(sd(ce3b$mean_distance) > 0)
# })

