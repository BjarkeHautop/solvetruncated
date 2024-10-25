#' Solve for parameters of a truncated normal distribution
#'
#' @param desired_mean The desired mean of the truncated normal distribution.
#' @param x_value The value where the CDF is evaluated.
#' @param desired_prob The desired value of the CDF at `x_value`.
#' @param a Lower value of the truncation. Default is \eqn{0}.
#' @param b Upper value of the truncation. Default is \eqn{\infty}.
#' @param initial_guess Initial guess for the numerical solver.
#' @param verbose Prints output of solver if TRUE.
#'
#' @return Returns a list of the parameters found.
#' @import stats
#' @export
#' @examples
#' ## Example use for desired mean 2 and P(X<=1)=0.3 with default truncation
#' solve_truncated_normal(2, 1, 0.3)
#'
#' ## Print output of optim by setting verbose=TRUE
#' solve_truncated_normal(2, 1, 0.3, verbose = TRUE)
solve_truncated_normal <- function(desired_mean,
                                   x_value,
                                   desired_prob,
                                   a = 0,
                                   b = Inf,
                                   initial_guess = c(desired_mean, 1),
                                   verbose = FALSE) {
  validate_inputs(desired_mean, x_value, desired_prob, a, b)

  objective_function <- function(params) {
    calculate_error(params, x_value, desired_mean, desired_prob, a, b)
  }

  result <- optim(initial_guess, objective_function)
  handle_optim_result_normal(result, verbose)
}

# Validates the inputs to the main function
validate_inputs <- function(desired_mean, x_value, desired_prob, a, b) {
  if (a >= b) {
    stop("Error: Lower bound 'a' must be less than upper bound 'b'.")
  }
  if (desired_mean <= a || desired_mean >= b) {
    stop("Error: Desired mean must be within the bounds [a, b].")
  }
  if (desired_prob <= 0 || desired_prob >= 1) {
    stop("Error: Desired probability must be between 0 and 1.")
  }
  if (x_value <= a || x_value >= b) {
    stop("Error: x_value must be within the bounds [a, b].")
  }
}

# Calculates the error for given parameters
# Follows syntax on https://en.wikipedia.org/wiki/Truncated_normal_distribution
calculate_error <- function(params, x_value, desired_mean, desired_prob, a, b) {
  mu <- params[1]
  sigma <- params[2]

  if (mu <= a || mu >= b || sigma <= 0) return(Inf)

  xi <- (x_value - mu) / sigma
  alpha <- (a - mu) / sigma
  beta <- (b - mu) / sigma
  z <- pnorm(beta) - pnorm(alpha)

  mean_trunc <- mu + (dnorm(alpha) - dnorm(beta)) / z * sigma
  prob_trunc <- (pnorm(xi) - pnorm(alpha)) / z

  abs(mean_trunc - desired_mean) + abs(prob_trunc - desired_prob)
}

# Handles the optimization result and prints if needed
handle_optim_result_normal <- function(result, verbose) {
  if (result$convergence != 0) {
    warning("Optimization did not converge.")
  }
  if (verbose) {
    print(result)
  }
  list(mu = result$par[1], sigma = result$par[2])
}
