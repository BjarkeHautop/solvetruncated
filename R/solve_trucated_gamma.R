#' Solve for parameters of a truncated gamma distribution
#'
#' @param desired_mean The desired mean of the truncated gamma distribution.
#' @param x_value The value where the CDF is evaluated.
#' @param desired_prob The desired value of the CDF at `x_value`.
#' @param a Lower value of the truncation. Default is \eqn{0}.
#' @param b Upper value of the truncation. Default is \eqn{\infty}.
#' @param initial_guess Initial guess for the numerical solver, in the form of shape and rate.
#' @param verbose Prints output of solver if TRUE.
#'
#' @return Returns a list of the parameters found.
#' @import stats
#' @export
#' @examples
#' ## Example use for desired mean 2 and P(X<=1)=0.3 with default truncation
#' solve_truncated_gamma(2, 1, 0.3)
#'
#' ## Print output of optim by setting verbose=TRUE
#' solve_truncated_gamma(2, 1, 0.3, verbose = TRUE)
solve_truncated_gamma <- function(desired_mean,
                                  x_value,
                                  desired_prob,
                                  a = 0,
                                  b = Inf,
                                  initial_guess = c(2, 1),
                                  verbose = FALSE) {
  validate_inputs(desired_mean, x_value, desired_prob, a, b)

  objective_function <- function(params) {
    calculate_error_gamma(params, x_value, desired_mean, desired_prob, a, b)
  }

  result <- optim(initial_guess, objective_function)
  handle_optim_result_gamma(result, verbose)
}

# Validates the inputs to the main function
validate_inputs <- function(desired_mean, x_value, desired_prob, a, b) {
  if (a >= b) {
    stop("Error: Lower bound 'a' must be less than upper bound 'b'.")
  }
  if (desired_prob <= 0 || desired_prob >= 1) {
    stop("Error: Desired probability must be between 0 and 1.")
  }
  if (x_value <= a || x_value >= b) {
    stop("Error: x_value must be within the bounds [a, b].")
  }
}

# Calculates the error for given parameters for a truncated gamma distribution
calculate_error_gamma <- function(params, x_value, desired_mean, desired_prob, a, b) {
  shape <- params[1]
  rate <- params[2]

  if (shape <= 0 || rate <= 0) {
    print(paste("Invalid parameters encountered: shape =", shape, ", rate =", rate))
    return(Inf)
  }

  # Calculate CDF values at the truncation points
  cdf_a <- if (a > 0) pgamma(a, shape, rate) else 0
  cdf_b <- if (is.finite(b)) pgamma(b, shape, rate) else 1
  z <- cdf_b - cdf_a

  # Calculate the truncated mean
  mean_trunc <- (shape / rate) * (pgamma(b, shape + 1, rate) - pgamma(a, shape + 1, rate)) / z

  # Calculate the truncated CDF at x_value
  cdf_x <- (pgamma(x_value, shape, rate) - cdf_a) / z

  # Objective function to minimize the absolute differences from desired values
  abs(mean_trunc - desired_mean) + abs(cdf_x - desired_prob)
}

# Handles the optimization result and prints if needed
handle_optim_result_gamma <- function(result, verbose) {
  if (result$convergence != 0) {
    warning("Optimization did not converge.")
  }
  if (verbose) {
    print(result)
  }
  list(shape = result$par[1], rate = result$par[2])
}
