# poolDatagen.R

### INPUTS...
# xobs: #the ode simulates xobs


poolDatagen <- function(xobs) {
  # generate monomials
  n <- nrow(xobs)
  d <- ncol(xobs)
  if (length(wsinit@polys) != 0) {
    P <- wsinit@polys[length(wsinit@polys)]
  } else {
    P <- 0
  }


  f <- function(t, x) {
    return(prod(t^x))
  }

  powers <- list()
  for (p in 1:P) {
    if (as.numeric(d) == 2) {
      size <- d + p - 1
      dp <- 0
      dq <- length(seq_len(size))-1
      for (indices in combinat::combn(seq_len(size), d-1)) {
        starts <- c(dp,dq)
        powers[[length(powers)+1]] <- as.list(starts)
        dp <- dp + 1
        dq <- dq - 1
      }
    }
    else if (as.numeric(d) == 1) {
      size <- d + p - 1
      dp <- 0
      starts <- c(p)
      powers[[length(powers)+1]] <- as.list(starts)
    }
    else if (as.numeric(d) == 3) {
      size <- d + p - 1
      dp <- 0
      dq <- 0
      dq2 <- 0
      dq3 <- 0
      dq4 <- 0
      dz <- p
      for (i in 1:dim(combinat::combn(seq_len(size), d-1))[2]) {

        if (i <= (p+1)) {
          start = 0
          stop <- dz
          mid <- p - stop - start
          dz <- dz - 1
        } else if (i > p+1 & i <= (p+1+p)) {
          start = 1
          mid <- dq
          stop <- p - start - mid
          dq <- dq +1
          pp <- p+1+p
        } else if (i > p+1+p && i <= (3*p)) {
          start = 2
          mid <- dq2
          stop <- p - start - mid
          dq2 <- dq2 + 1

          pp <- 3*p
        } else if (i > (3*p) && i <= (4*p - 2)) {
          start = 3
          mid <- dq3
          stop <- p - start - mid
          dq3 <- dq3 + 1
          pp <- 4*p - 2
        } else if (i > (4*p - 2) && i <= (5*p-5)) {
          start = 4
          mid <- dq4
          stop <- p - start - mid
          dq4 <- dq4 + 1
          pp <- 5*p-5
        } else if (i > (5*p-5) && i <= (6*p - 7)) {
          start = 5
          mid <- 0
          stop <- 0
        } else {
          print("Please Select Polys to be seq. of 0 to 5")
        }

        pwrs <- c(start,mid,stop) #get powers array
        powers[[length(powers)+1]] <- as.list(pwrs)

      }
    }
  }

  theta_0 <- matrix(1, nrow = n, ncol = 1)

  for (k in 1:length(powers)) {
    new_column <- matrix(0, nrow = n, ncol = 1)
    for (i in 1:n) {
      if (as.numeric(d) == 2) {
        new_column[i] <- f(xobs[i,], c(powers[[k]][[1]],powers[[k]][[2]]))
      } else if (as.numeric(d) == 1) {
        new_column[i] <- f(xobs[i,], c(powers[[k]][[1]]))
      } else if (as.numeric(d) == 3) {
        new_column[i] <- f(xobs[i,], c(powers[[k]][[1]],powers[[k]][[2]],
                           powers[[k]][[3]]))
      }

    }
    theta_0 <- cbind(theta_0, new_column)
  }

  # trigs:
  #initalize tags matrix
  tags <- matrix(unlist(powers), nrow = length(powers), ncol = d, byrow = TRUE)
  #changed ncol to be d, but may have to change back

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

  anslist <- list("theta_0" = theta_0,
                  "tags" = tags)
  return(anslist)
}
