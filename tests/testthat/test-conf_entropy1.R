set.seed(2021-10-11)
ce1 = conf_entropy1(mosaic, 1000)

test_that("conf_entropy1 works", {
  expect_equal(nrow(ce1), 12)
  expect_equal(ncol(ce1), 7)
  expect_equal(ce1$mean_total_edge[[1]], 241.177)
  expect_equal(ce1$sd_total_edge[[1]], 10.69, tolerance = 0.001)
  expect_equal(ce1$lnp[[1]], -3.32, tolerance = 0.001)
  expect_equal(ce1$total_edge[[1]], 244)
})

ce1b = conf_entropy1(mosaic, 1000, independent = TRUE)

test_that("conf_entropy1 works with independent=TRUE", {
  expect_true(sd(ce1b$mean_total_edge) > 0)
})

