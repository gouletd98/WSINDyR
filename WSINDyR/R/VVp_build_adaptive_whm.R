# VVp_build_adaptive_whm.R

### INPUTS...
# t:
# centers:
# r_whm:
# param:

VVp_build_adaptive_whm <- function(t, centers, r_whm, param = NULL) {
  if (is.null(param)) {
    param <- c(1, 2, 1)
  }

  N <- length(t)
  M <- length(centers)
  V <- matrix(0, nrow=M, ncol=N)
  Vp <- matrix(0, nrow=M, ncol=N)
  ab_grid <- matrix(0, nrow=M, ncol=2)
  ps <- matrix(0, nrow=M, ncol=1)
  pab <- test_fcn_param(r_whm, t[as.integer(centers[1])-1], t)

  a <- as.integer(pab[2])
  b <- as.integer(pab[3])

  if (b - a < 10) {
    center <- (a + b) / 2
    a <- max(0, floor(center - 5))
    b <- min(ceiling(center + 5), length(t))
  }

  gb <- basis_fcn(pab[1], pab[1])
  V_row <- tf_mat_row(gb[[1]], gb[[2]], t, a, b, param)

  V[1, ] <- V_row
  Vp[1, ] <- tf_mat_row(gb[[1]], gb[[2]], t, a, b, c(1, 2, 2))
  ab_grid[1, ] <- c(a, b)
  ps[1] <- pab[1]

  for (k in 2:M) {
    cent_shift <- as.integer(centers[k] - centers[k-1])
    b_temp <- min(b + cent_shift, length(t))

    if (a > 1 && b_temp < length(t)) {
      a <- a + cent_shift
      b <- b_temp
      V_row <- c(rep(0, cent_shift), V_row[1:(N - cent_shift)])
      Vp_row <- c(rep(0, cent_shift), Vp_row[1:(N - cent_shift)])
    } else {
      pab <- test_fcn_param(r_whm, t[as.integer(centers[k])-1], t)
      a <- as.integer(pab[2])
      b <- as.integer(pab[3])

      if (b - a < 10) {
        center <- (a + b) / 2
        b <- min(ceiling(center + 5), length(t))
        a <- max(0, floor(center - 5))
      }

      gb <- basis_fcn(pab[1], pab[1])
      V_row <- tf_mat_row(gb[[1]], gb[[2]], t, a, b, param)
      Vp_row <- tf_mat_row(gb[[1]], gb[[2]], t, a, b, c(1, 2, 2))
    }

    V[k, ] <- V_row
    Vp[k, ] <- Vp_row
    ab_grid[k, ] <- c(a, b)
    ps[k] <- pab[1]
  }

  return(list(V=V, Vp=Vp, ab_grid=ab_grid))
}
