sample_truncated_normal <- function(n, mu, sigma, a = 0, b = Inf) {
  samples <- numeric(n)
  count <- 0

  while (count < n) {
    x <- rnorm(1, mean = mu, sd = sigma)

    # Accept-reject condition
    if (x >= a && x <= b) {
      count <- count + 1
      samples[count] <- x
    }
  }

  return(samples)
}

# Test for the truncated normal solver
test_that("Truncated normal mean and CDF matches desired values", {
  desired_mean <- 2
  x_value <- 1
  desired_prob <- 0.3
  a <- 0
  b <- Inf
  n_samples <- 10^5

  # Calculate parameters using your function
  params <- solve_truncated_normal(desired_mean, x_value, desired_prob, a, b)
  mu <- params$mu
  sigma <- params$sigma

  # Sample from the truncated normal distribution
  samples <- sample_truncated_normal(n_samples, mu, sigma, a, b)

  # Calculate the sampled mean
  sampled_mean <- mean(samples)

  # Calculate the probability at x_value
  prob_at_x_value <- mean(samples <= x_value)

  # Check if the sampled mean is close to the desired mean
  expect_equal(sampled_mean, desired_mean, tolerance = 0.01)  # Adjust tolerance as needed

  # Check if the probability at x_value is close to the desired probability
  expect_equal(prob_at_x_value, desired_prob, tolerance = 0.01)  # Adjust tolerance as needed
})
