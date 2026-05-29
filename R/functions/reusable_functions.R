# Script - reusable_functions.R
# Task - Holds several reused functions that are applied in several scripts
# in the pipeline
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)

# Functions to transform natural scale to log+1 scale and back again
logplus1 <- function(x) log(x+1)
unlogplus1 <- function(x) exp(x)-1

rmse <- function(actual, predicted) {
  sqrt(mean((predicted - actual)^2, na.rm = TRUE))
}

T_dep <- function (sbt, Topt, width){
  return (exp(-0.5 * ((sbt - Topt)/width)^2)) # gaussian temperature-dependent function
}

# Log-Quadratic temperature-dependent function
T_dep_log_mortality <- function(sbt, beta_0, beta_1, beta_2){
  M <- (log(beta_0) + log(beta_1 * sbt) + log(beta_2 * sbt^2))
  return (M)
}

T_dep_exp_mortality <- function(sbt, beta_0, beta_1, beta_2){
  M <- exp(beta_0) + exp(beta_1)*sbt + exp(beta_2)*sbt^2
  return (M)
}

T_dep_full_log_mortality <- function(sbt, beta_0, beta_1, beta_2){
  M <- exp(log(beta_0 + beta_1 * sbt + beta_2 * sbt^2))
  return (M)
}

T_dep_mortality_func <- function(sbt, beta_0, beta_1, beta_2){
  M <- beta_0 + beta_1 * sbt + beta_2 * sbt^2
  return (M)
}
