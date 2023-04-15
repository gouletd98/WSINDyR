# poolDatagen.R

### INPUTS...
# xobs:
install.packages("combinat")
library(combinat)

# NOTE - "self" needs to be defined manually
# replace with "obj" or "object"? still not sure

poolDatagen <- function(xobs) {
  # generate monomials
  n <- nrow(xobs)
  d <- ncol(xobs)
  if (length(wsinit@polys) != 0) {
    P <- wsinit@polys[length(wsinit@polys)]
  } else {
    P <- 0
  }

  rhs_functions <- list()

  f <- function(t, x) {
    prod(t^x)
  }

  ##LEFT OFF HERE *********************************
  powers <- list()
  for (p in 1:P) {
    size <- d + p - 1
    for (indices in combinat::combn(seq_len(size), d-1)) {
      starts <- c(0, indices + 1)
      stops <- indices + size #c(indices, size)
      powers[[length(powers) + 1]] <- as.list(stops - starts)
    }
  }

  for (power in 1:length(powers)) {
    # rhs_functions[[as.character(powers)]] <- list(f = function(t, x = power) { f(t, x) }, power)
    rhs_functions[[as.character(power)]] <- list(f = function(t, x = power) { f(t, x) }, power)
    # rhs_functions[[power]] <- list(function(t, x=power) {
    #   f(t, x)
    #   },
    #   power)
  }

  theta_0 <- matrix(1, nrow = n, ncol = 1)
  # print(powers)

  tags <- do.call(rbind, lapply(powers, function(p) p$power))
  # print('tags', tags)
  # plug in
  for (k in seq_along(rhs_functions)) {
    func <- rhs_functions[[k]]$f
    new_column <- matrix(0, nrow = n, ncol = 1)
    for (i in 1:n) {
      new_column[i] <- func(xobs[i,])
    }
    theta_0 <- cbind(theta_0, new_column)
  }

  # trigs:
  for (i in seq_along(wsinit@trigs)) {
    trig_inds <- rbind(-wsinit@trigs[i]*1i*matrix(1, ncol = d), wsinit@trigs[i]*1i*matrix(1, ncol = d))
    sin_col <- matrix(0, nrow = n, ncol = 1)
    cos_col <- matrix(0, nrow = n, ncol = 1)
    for (m in 1:n) {
      sin_col[m] <- sin(wsinit@trigs[i] * xobs[m,])
      cos_col[m] <- cos(wsinit@trigs[i] * xobs[m,])
    }
    theta_0 <- cbind(theta_0, sin_col, cos_col)
    tags <- rbind(tags, trig_inds)
  }

  tags <- rbind(matrix(0, nrow = 1, ncol = d), tags)
  # print(tags)
  anslist <- list("theta_0" = theta_0,
                  "tags" = tags)
  return(anslist)
}
