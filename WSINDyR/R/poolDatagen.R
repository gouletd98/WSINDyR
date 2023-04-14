# poolDatagen.R

### INPUTS...
# xobs:

# NOTE - "self" needs to be defined manually
# replace with "obj" or "object"? still not sure

poolDatagen <- function(self, xobs) {
  # generate monomials
  n <- nrow(xobs)
  d <- ncol(xobs)
  if (length(self$polys) != 0) {
    P <- self$polys[length(self$polys)]
  } else {
    P <- 0
  }
  rhs_functions <- list()

  f <- function(t, x) {
    prod(sapply(seq_along(x), function(i) t[x[i]]))
  }

  powers <- list()
  for (p in 1:P) {
    size <- d + p - 1
    for (indices in combn(seq_len(size), d-1)) {
      starts <- c(0, indices + 1)
      stops <- c(indices, size)
      powers[[length(powers) + 1]] <- list(power = stops - starts, indices = indices)
    }
  }
  for (power in powers) {
    rhs_functions[[as.character(power$power)]] <- list(f = function(t, x = power$power) f(t, x), power = power$power)
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
  for (i in seq_along(self$trigs)) {
    trig_inds <- rbind(-self$trigs[i]*1i*matrix(1, ncol = d), self$trigs[i]*1i*matrix(1, ncol = d))
    sin_col <- matrix(0, nrow = n, ncol = 1)
    cos_col <- matrix(0, nrow = n, ncol = 1)
    for (m in 1:n) {
      sin_col[m] <- sin(self$trigs[i] * xobs[m,])
      cos_col[m] <- cos(self$trigs[i] * xobs[m,])
    }
    theta_0 <- cbind(theta_0, sin_col, cos_col)
    tags <- rbind(tags, trig_inds)
  }

  tags <- rbind(matrix(0, nrow = 1, ncol = d), tags)
  # print(tags)
  return(list(theta_0, tags))
}
