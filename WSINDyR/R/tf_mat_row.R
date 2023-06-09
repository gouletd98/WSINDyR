# tf_mat_row.R

### INPUTS...
# g:
# gp:
# t:
# t1:
# tk:
# param: defines the normalization parameter, must be in correct syntax

tf_mat_row <- function(g, gp, t, t1, tk, param) {

  # function code here
  N <- length(t)

  if (is.null(param) == TRUE) { #checking if the parameter is null or not
    pow <- 1
    gap <- 1
    nrm <- "I"
    ord <- 0
  } else {
    pow <- as.numeric(param[1])
    nrm <- param[2] #This should be "I" for the infinite norm!
    ord <- as.numeric(param[3])
    gap <- 1
  }

  if (t1 > tk) {
    tk_temp <- tk
    tk <- t1
    t1 <- tk_temp
  }
  #need to add clause for if t1 <- 0
  if (t1 == 0) {
    t1 <- 1 #gets indexing straight
  }
  V_row <- matrix(0, nrow = 1, ncol = N) #rep(0,N)
  Vp_row <- V_row #hopefully does not point to it like python

  t_grid <- t[seq(t1,(tk+1),gap)]
  dts <- diff(t_grid) #time steps
  w <- 0.5*(append(dts,0) + append(0, dts)) #appends 0 to dts and dts to 0

  V_row[, seq(t1,(tk+1),gap)] = g(t_grid, t[t1], t[tk])*w
  Vp_row[, seq(t1,(tk+1),gap)] = -gp(t_grid, t[t1], t[tk])*w
  Vp_row[,t1] = Vp_row[,t1] - g(t[t1], t[t1], t[tk])
  Vp_row[,tk] = Vp_row[,tk] + g(t[t1], t[t1], t[tk])

  if (pow != 0) {
    if (!is.na(ord) && ord == 0) {
      scale_fac <- norm(as.vector(V_row[,seq(t1,(tk+1),gap)]),
                       nrm)
    } else if (!is.na(ord) && ord == 1) {
      scale_fac <- norm(as.vector(Vp_row[,seq(t1,(tk+1),gap)]),
                        nrm)
    } else {
      scale_fac <- mean(dts)
    }

    V_row <- V_row/scale_fac
    Vp_row <- Vp_row/scale_fac

  } else {
    # do nothing
  }

  # now get everything into compiled state
  anslist <- list("V_row" = V_row,
                  "Vp_row" = Vp_row)
  return(anslist)
}
