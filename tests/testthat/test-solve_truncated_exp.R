# Sample function from the truncated exponential distribution
sample_truncated_exponential <- function(n, lambda, a = 0, b = Inf,
                                         oversample_factor = 2) {
  # Initial estimate of how many samples to generate
  num_samples_needed <- n
  samples <- numeric(0)

  while (length(samples) < n) {
    # Generate more samples than needed (oversampling)
    x <- rexp(num_samples_needed * oversample_factor, rate = lambda)

    # Filter samples that fall within the truncation range
    x <- x[x >= a & x <= b]

    # Collect the accepted samples
    samples <- c(samples, x)

    # Update the number of samples needed
    num_samples_needed <- n - length(samples)
  }

  return(samples[1:n])
}

# Test for the truncated exponential solver
test_that("Truncated exponential mean matches desired mean", {
  set.seed(1405)
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
  expect_equal(sampled_mean, desired_mean, tolerance = 0.01)
})
