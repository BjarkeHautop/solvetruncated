
<!-- README.md is generated from README.Rmd. Please edit that file -->

# solvetruncated

<!-- badges: start -->
<!-- badges: end -->

The goal of solvetruncated is to solve for parameters of truncated
distributions given desired mean and value of CDF at a point. It does
this by numerical optimization.

This is for example useful for when specifying a prior in Bayesian,
where your belief is often with respect to a mean and value of CDF at a
point.

## Installation

You can install the development version of solvetruncated from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BjarkeHautop/solvetruncated")
```

## Example

Here is an example where we wish to solve for the parameters
$(\mu, \sigma^2)$ of
$$X \sim \text{TruncNormal}(\mu, \, \sigma^2, \, a=0, \, b=\infty),$$
where we wish $E[X]=1.5$ and $P(X\leq 2)=0.8$.

``` r
library(solvetruncated)
solve_truncated_normal(desired_mean=1.5, x_value=2, desired_prob=0.8, a=0, b=Inf)
#> $mu
#> [1] 1.488279
#> 
#> $sigma
#> [1] 0.6044566
```

which gives that the parameters should be $(\mu, \sigma^2)$.
