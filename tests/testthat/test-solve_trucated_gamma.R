# Function to sample from a truncated gamma distribution
sample_truncated_gamma <- function(n, shape, rate, a = 0, b = Inf,
                                   oversample_factor = 2) {
  # Initial estimate of how many samples to generate
  num_samples_needed <- n
  samples <- numeric(0)

  while (length(samples) < n) {
    if (shape <= 0) {
      print("shape <=0")
    }
    if (rate <= 0) {
      print("rate <=0")
    }
    # Generate more samples than needed (oversampling)
    x <- rgamma(num_samples_needed * oversample_factor,
                shape = shape,
                rate = rate)

    # Filter samples that fall within the truncation range
    x <- x[x >= a & x <= b]

    # Collect the accepted samples
    samples <- c(samples, x)

    # Update the number of samples needed
    num_samples_needed <- n - length(samples)
  }

  return(samples[1:n])
}

# Test for the truncated gamma solver
test_that("Truncated gamma mean and CDF matches desired values", {
  set.seed(1405)
  desired_mean <- 3
  x_value <- 1
  desired_prob <- 0.3
  a <- 0
  b <- Inf
  n_samples <- 10^5

  # Calculate parameters using your function
  params <- solve_truncated_gamma(desired_mean, x_value, desired_prob, a, b)
  shape <- params$shape
  rate <- params$rate

  # Sample from the truncated gamma distribution
  samples <- sample_truncated_gamma(n_samples, shape, rate, a, b)

  # Calculate the sampled mean
  sampled_mean <- mean(samples)

  # Calculate the probability at x_value
  prob_at_x_value <- mean(samples <= x_value)

  # Check if the sampled mean is close to the desired mean
  expect_equal(sampled_mean, desired_mean, tolerance = 0.1)

  # Check if the probability at x_value is close to the desired probability
  expect_equal(prob_at_x_value, desired_prob, tolerance = 0.1)
})
