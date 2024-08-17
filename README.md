
<!-- README.md is generated from README.Rmd. Please edit that file -->

# solvetruncated

<!-- badges: start -->

[![R-CMD-check](https://github.com/BjarkeHautop/solvetruncated/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BjarkeHautop/solvetruncated/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Overview

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
$(\mu, \sigma)$ of
$$X \sim \text{TruncNormal}(\mu, \sigma, a=0, b=\infty),$$ where we wish
to have $E[X]=0.5$ and $P(X\leq 0.75)=0.8$.

``` r
library(solvetruncated)
solve_truncated_normal(desired_mean = 0.5, x_value = 0.75, desired_prob = 0.8, a = 0, b = Inf)
#> $mu
#> [1] 0.4208603
#> 
#> $sigma
#> [1] 0.3543778
```

which gives that the parameters should be $(\mu=0.42, \sigma=0.35)$.
