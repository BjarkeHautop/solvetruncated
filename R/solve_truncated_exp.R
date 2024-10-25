#' Solve for the rate parameter of a truncated exponential distribution
#'
#' @param desired_mean The desired mean of the truncated exponential distribution.
#' @param a Lower value of the truncation. Default is \eqn{0}.
#' @param b Upper value of the truncation. Default is \eqn{\infty}.
#' @param initial_guess Initial guess for the rate parameter. Default is \eqn{1 / (desired\_mean - a)}.
#' @param verbose Prints output of solver if TRUE.
#'
#' @return Returns the rate parameter found.
#' @import stats
#' @export
#' @examples
#' ## Example use for desired mean 3 with lower truncation at 1 and upper
#' ## truncation at 100
#' solve_truncated_exponential(1, 3, 100)
#'
#' ## Print output of optim by setting verbose=TRUE
#' solve_truncated_exponential(1, verbose = TRUE)
solve_truncated_exponential <- function(desired_mean,
                                        a = 0,
                                        b = Inf,
                                        initial_guess = 1 / (desired_mean - a),
                                        verbose = FALSE) {
  validate_inputs_exp(desired_mean, a, b)

  # Objective function to minimize
  objective_function <- function(lambda) {
    calculate_error_exp(lambda, desired_mean, a, b)
  }

  if (is.infinite(b)) {
    # For b = Inf, calculate lambda directly
    lambda <- 1 / (desired_mean - a)
    return(lambda)
  } else {
    # Use Brent's method optimization
    result <- optim(initial_guess, objective_function, method = "Brent",
                    lower = 0, upper = max(1000, 1 / desired_mean))
  }

  handle_optim_result_exp(result, verbose)
}

# Validates the inputs to the main function
validate_inputs_exp <- function(desired_mean, a, b) {
  if (a >= b) {
    stop("Error: Lower bound 'a' must be less than upper bound 'b'.")
  }
}

# Calculates the error for a given rate parameter
calculate_error_exp <- function(lambda, desired_mean, a, b) {
  if (lambda <= 0) return(Inf)

  # Calculate the mean of the truncated exponential distribution
  if (is.infinite(b)) {
    # For b = Inf
    mean_trunc <- a + (1 / lambda)
  } else {
    # General case
    mean_trunc <- (1 / lambda) + (a * exp(-lambda * a) - b * exp(-lambda * b)) /
      (exp(-lambda * a) - exp(-lambda * b))
  }

  # Return the absolute error between calculated mean and desired mean
  abs(mean_trunc - desired_mean)
}

# Handles the optimization result and prints if needed
handle_optim_result_exp <- function(result, verbose) {
  if (result$convergence != 0) {
    warning("Optimization did not converge.")
  }
  if (verbose) {
    print(result)
  }
  return(result$par)
}
