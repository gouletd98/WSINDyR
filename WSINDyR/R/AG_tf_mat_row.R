# AG_tf_mat_row.R

### INPUTS...
# g:
# gp:
# t:
# t1:
# tk:
# param:

AG_tf_mat_row <- function(g, gp, t, t1, tk, param = NULL) {
  N <- length(t)

  if (is.null(param)) {
    gap <- 1
    nrm <- Inf
    ord <- 0
  } else {
    gap <- param[1]
    nrm <- param[2]
    ord <- param[3]
  }

  if (t1 > tk) {
    tk_temp <- tk
    tk <- t1
    t1 <- tk_temp
  }

  V_row <- matrix(0, nrow=1, ncol=N)
  Vp_row <- matrix(V_row)

  t_grid <- t[t1:(tk+1):gap]
  dts <- diff(t_grid)
  w <- 1/2 * (c(dts, 0) + c(0, dts))

  V_row[, t1:(tk+1):gap] <- g(t_grid, t[t1], t[tk]) * w
  Vp_row[, t1:(tk+1):gap] <- -gp(t_grid, t[t1], t[tk]) * w
  Vp_row[, t1] <- Vp_row[, t1] - g(t[t1], t[t1], t[tk])
  Vp_row[, tk] <- Vp_row[, tk] + g(t[tk], t[t1], t[tk])

  if (ord == 0) {
    scale_fac <- norm(V_row[, t1:(tk+1):gap], nrm)
  } else if (ord == 1) {
    scale_fac <- norm(Vp_row[, t1:(tk+1):gap], nrm)
  } else {
    scale_fac <- mean(dts)
  }
  Vp_row <- Vp_row/scale_fac
  V_row <- V_row/scale_fac
  list(V_row, Vp_row)
}
