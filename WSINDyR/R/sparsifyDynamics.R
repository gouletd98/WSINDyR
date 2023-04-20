# sparsifyDynamics.R

### INPUTS...
# Theta:
# dXdt:
# n:
# M:

sparsifyDynamics <- function(Theta, dXdt, n, M = NULL) {
  if (is.null(M) == TRUE) {
    # M <- matrix(1, nrow = length(Theta), ncol = 1)
    M <- matrix(1, nrow = ncol(Theta), ncol = 1)
  }

  if (wsinit@gamma == 0) {
    Theta_reg <- Theta
    dXdt_reg <- matrix(dXdt, nrow = length(dXdt), ncol = 1)
  } else {
    nn <- ncol(Theta) #length(Theta)
    Theta_reg <- rbind(Theta, wsinit@gamma * diag(nn))
    dXdt <- matrix(dXdt, nrow = length(dXdt), ncol = 1)
    dXdt_reg_temp <- rbind(dXdt, wsinit@gamma * matrix(0, nrow = nn, ncol = n))
    dXdt_reg <- matrix(dXdt_reg_temp, nrow = length(dXdt_reg_temp), ncol = 1)
  }


  # Xi <- M * qr.solve(Theta_reg, dXdt_reg)

  #CURRENT
  # Ximod <- lsfit(Theta_reg,dXdt_reg, intercept = FALSE)
  # Xi <- M * as.matrix(Ximod$coefficients[])

  A <- Theta_reg
  B <- dXdt_reg

  Ximod <- solve(t(A) %*% A) %*% t(A) %*% B
  Xi <- M * Ximod

  #Xi <- M * solve(Theta_reg, dXdt_reg)

  for (i in 1:10) {
    smallinds <- as.numeric(abs(Xi) < wsinit@ld)
    while (sum(smallinds) == length(Xi)) {
      wsinit@ld <- wsinit@ld / 2
      smallinds <- as.numeric(abs(Xi) < wsinit@ld)
    }
    Xi[smallinds] <- 0
  }

  for (ind in 1:n) {
    biginds <- as.numeric(!smallinds[ind])
    temp <- dXdt_reg[, ind]
    temp <- matrix(temp, nrow = length(temp), ncol = 1)
    # xitrm2 <- lsfit(Theta_reg[, biginds], temp, intercept = FALSE)
    # Xi[biginds, ind] <- M[biginds] %*% as.matrix(xitrm2$coefficients[])

    #CURRENT
    Xi[biginds, ind] <- M[biginds] %*% qr.solve(Theta_reg[, biginds], temp)

    # xitrm2 <- solve(t(Theta_reg[, biginds]) %*% Theta_reg[, biginds]) %*%
    #   t(Theta_reg[, biginds]) %*% temp
    #
    # Xi[biginds, ind] <- M[biginds] %*% xitrm2
  }

  anslist <- list("Xi" = Xi)
  return(anslist)
}

