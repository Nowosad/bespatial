set.seed(2021-10-11)
m1 = matrix(1, nrow = 7, ncol = 7)
m2 = matrix(c(rep(1, 16), rep(2, 14), rep(3, 12), rep(4, 7)),
            nrow = 7, ncol = 7)
m3 = matrix(c(1, 3, 1, 3, 1, 2, 2,
              3, 1, 3, 1, 2, 1, 2,
              1, 1, 1, 1, 2, 1, 1,
              3, 3, 3, 3, 3, 2, 1,
              4, 4, 3, 3, 2, 1, 2,
              4, 4, 3, 1, 2, 2, 1,
              2, 2, 2, 2, 4, 4, 4), nrow = 7, ncol = 7)
m4 = matrix(c(2, 4, 1, 3, 4, 1, 3,
              3, 1, 3, 2, 1, 3, 2,
              3, 4, 1, 4, 3, 1, 1,
              1, 3, 2, 1, 3, 1, 3,
              3, 2, 4, 1, 2, 2, 2,
              2, 2, 4, 3, 4, 1, 2,
              1, 2, 1, 2, 1, 2, 1), nrow = 7, ncol = 7)
m5 = matrix(1:49, nrow = 7, ncol = 7)

zhao1 = bes_m_zhao(m1)
zhao2 = bes_m_zhao(m2)
zhao3 = bes_m_zhao(m3)
zhao4 = bes_m_zhao(m4)
zhao5 = bes_m_zhao(m5)

test_that("bes_m_zhao works", {
  expect_equal(nrow(zhao2), 1)
  expect_equal(ncol(zhao2), 4)
  expect_equal(c(zhao1$value, zhao2$value, zhao3$value, zhao4$value, zhao5$value),
               c(0, 0.1733, 0.3219, 0.3849, 1), tolerance = 0.001)
})

