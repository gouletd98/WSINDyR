# sparsifyDynamics.R

### INPUTS...
# Theta:
# dXdt:
# n:
# M:

# NOTE - 'self' parameter was not defined
# need to figure out proper formatting for that

sparsifyDynamics <- function(Theta, dXdt, n, M = NULL) {
  if (is.null(M)) {
    M <- matrix(1, nrow = ncol(Theta), ncol = 1)
  }

  if (self$gamma == 0) {
    Theta_reg <- Theta
    dXdt_reg <- matrix(dXdt, nrow = length(dXdt), ncol = 1)
  } else {
    nn <- ncol(Theta)
    Theta_reg <- rbind(Theta, self$gamma * diag(nn))
    dXdt <- matrix(dXdt, nrow = length(dXdt), ncol = 1)
    dXdt_reg_temp <- rbind(dXdt, self$gamma * matrix(0, nrow = nn, ncol = n))
    dXdt_reg <- matrix(dXdt_reg_temp, nrow = length(dXdt_reg_temp), ncol = 1)
  }

  Xi <- M %*% solve(Theta_reg, dXdt_reg)

  for (i in 1:10) {
    smallinds <- abs(Xi) < self$ld
    while (sum(smallinds) == length(Xi)) {
      self$ld <- self$ld / 2
      smallinds <- abs(Xi) < self$ld
    }
    Xi[smallinds] <- 0
  }

  for (ind in 1:n) {
    biginds <- !smallinds[, ind]
    temp <- dXdt_reg[, ind]
    temp <- matrix(temp, nrow = length(temp), ncol = 1)
    Xi[biginds, ind] <- M[biginds] %*% solve(Theta_reg[, biginds], temp)
  }

  return(Xi)
}
