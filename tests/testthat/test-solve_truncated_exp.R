# Sample function from the truncated exponential distribution
sample_truncated_exponential <- function(n, lambda, a = 0, b = Inf) {
  samples <- numeric(n)
  count <- 0

  while (count < n) {
    x <- rexp(1, rate = lambda)

    if (x >= a && x <= b) {
      count <- count + 1
      samples[count] <- x
    }
  }

  return(samples)
}

# Test for the truncated exponential solver
test_that("Truncated exponential mean matches desired mean", {
  desired_mean <- 3
  a <- 1
  b <- 100
  n_samples <- 10^5

  # Calculate the rate parameter
  lambda <- solve_truncated_exponential(desired_mean, a, b)

  # Sample from the truncated exponential distribution
  samples <- sample_truncated_exponential(n_samples, lambda, a, b)

  # Calculate the sampled mean
  sampled_mean <- mean(samples)

  # Check if the sampled mean is close to the desired mean
  expect_equal(sampled_mean, desired_mean, tolerance = 0.01)  # Adjust tolerance as needed
})
