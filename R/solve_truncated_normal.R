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
#' solve_truncated_normal(2, 1, 0.3, verbose=TRUE)
solve_truncated_normal <- function(desired_mean, x_value, desired_prob, a = 0,
                                   b = Inf, initial_guess=c(desired_mean, 1),
                                   verbose=FALSE) {
  "
  This function solves for the parameters (mean and standard deviation) of a truncated normal distribution

  such that the distribution aligns with a given desired mean and the cumulative probability at a specific value.

  Args:
  desired_mean (numeric): The desired mean E[X] of the truncated normal distribution.
  desired_prob (numeric): The desired cumulative probability P(X <= x_value) of the truncated normal distribution.
  x_value (numeric): The value x for which the cumulative probability is specified.
  a (numeric, optional): The lower bound of the truncation interval. Default is 0.
  b (numeric, optional): The upper bound of the truncation interval. Default is Inf.
  Initial_guess: The initial guess for the parameters used for optimizing. Default
  is mean being the desired mean and sd being 1.
  maxit: The maximum number of iterations allowed for optim. See also ?optim.

  Returns:
  list: A list containing the optimal mean (mu) and standard deviation (sigma) of the truncated normal distribution.
  "

  # Check for logical consistency and edge cases
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

  # Define the objective function to minimize
  objective_function <- function(params) {
    mu <- params[1]
    sigma <- params[2]

    # Penalize if parameters go out of bounds
    if (mu <= a || mu >= b || sigma <= 0) {
      return(Inf)
    }

    # Following syntax given on https://en.wikipedia.org/wiki/Truncated_normal_distribution
    xi <- (x_value - mu) / sigma
    alpha <- (a - mu) / sigma
    beta <- (b - mu) / sigma
    Z <- pnorm(beta) - pnorm(alpha)

    mean_trunc <- mu + (dnorm(alpha) - dnorm(beta)) / Z * sigma
    prob_trunc <- (pnorm(xi) - pnorm(alpha)) / Z

    # Calculate the sum of squared differences
    error <- (mean_trunc - desired_mean)^2 + (prob_trunc - desired_prob)^2

    return(error)
  }

  # Optimize the parameters
  result <- optim(initial_guess, objective_function)

  # Check for convergence or failure
  if (result$convergence != 0) {
    warning("Optimization did not converge.")
  }

  if (verbose) {
    print(result)
  }

  # Extract the optimal parameters
  optimal_mu <- result$par[1]
  optimal_sigma <- result$par[2]

  return(list(mu = optimal_mu, sigma = optimal_sigma))
}
