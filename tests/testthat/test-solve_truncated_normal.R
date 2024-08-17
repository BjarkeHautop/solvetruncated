test_that("Check it finds parameters correctly", {
  expect_equal(solve_truncated_normal(1.2876, 2, 0.8114266), list(mu = 1, sigma = 1),
    tolerance = 1e-5
  )
})
