
# pakete ------------------------------------------------------------------
library(tidyverse)

# Likelhood functions -----------------------------------------------------

mean <- 0
sd <- 1

dichte_normal <- function(mean, sd){
  x <- seq(-5, 5, 0.001)
  y <- dnorm(x, mean, sd)
  return(data.frame(x, y))
}

likelihood_norma_sd <- function(x, mean){
  sd <- seq(0.00001, 10, 0.001)
  y <- 1/(sqrt(2*pi)*sd) * exp(-0.5*(x - mean)^2*(1/sd^2))
  return(data.frame(y, sd))
}
likelihood_exp <- function(x) {
  lambda <- seq(0.001, 10, 0.001)
  y <- lambda*exp(-lambda*x)
  return(data.frame(y, lambda))
}


# Visualization -----------------------------------------------------------
values_density <- dichte_normal(mean, sd)

x <- 0.5
values_likelihood <- likelihood_norma_sd(x, mean)

ggplot(values_density, aes(x, y = y)) +
  geom_line()

ggplot(values_likelihood, aes(x = sd, y = y)) +
  geom_line()


# Likelihood exponential distribution
x <- 2
values_likelihood_exp <- likelihood_exp(x)
ggplot(values_likelihood_exp, aes(x = lambda, y)) + 
  geom_line()



# Product of Likelihoods (SD) ---------------------------------------------
product_likeli_sd <- function(n, mean_start, sd_start) {
  y <- 1
  for (i in 1:n) {
    x <- rnorm(1, mean = mean_start, sd = sd_start)
    values_likelihood <- likelihood_norma_sd(x, mean_start)
    y <- y*values_likelihood$y
  }
  return(data.frame(x = values_likelihood$sd, y))
}

values_prod_likeli_sd <- product_likeli_sd(30, 0, 2)

ggplot(values_prod_likeli_sd, aes(x = x, y = y)) + 
  geom_line()




# Product of Likelihoods (Exponential, Lambda) ----------------------------

product_lambda <- function(n, lambda_start) {
  y <- 1
  for (i in 1:n) {
    x <- rexp(1, lambda_start)
    values_likelihood <- likelihood_exp(x)
    y <- y*values_likelihood$y
  }
  return(data.frame(x = values_likelihood$lambda, y))
}

values_prod_lambda <- product_lambda(30, 3)

ggplot(values_prod_lambda, aes(x = x, y = y)) + 
  geom_line()

index <- which.max(values_prod_lambda$y)
values_prod_lambda[index, ]

# Numerical Integration ---------------------------------------------------
integrate(splinefun(y = values_prod_lambda$y, x = values_prod_lambda$x), 0, 10)
integrate(approxfun(y = values_prod_lambda$y, x = values_prod_lambda$x), 0, 10)



