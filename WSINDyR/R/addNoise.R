# addNoise.R - adds noise to the ODE solutions

addNoise <- function(x, noise_ratio) {
  signal_power <- sqrt(mean(x^2))
  sigma <- noise_ratio*signal_power
  noise <- rnorm(length(x), mean = 0, sd = sigma) # generate normal dist.
  xobs <- x + noise
  return(xobs) # gets observations in data structure
}
