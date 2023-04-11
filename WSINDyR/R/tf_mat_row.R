# tf_mat_row.R

### INPUTS...
# g:
# gp:
# t:
# t1:
# tk:
# param: Defines the normalization parameter, must be in correct syntax

tf_mat_row <- function(g, gp, t, t1, tk, param) {

  # function code here
  N <- length(t)

  if (param == "None") {
    pow <- 1
    gap <- 1
    nrm <- "I"
    ord <- 0
  } else {
    pow <- param[1]
    nrm <- param[2] #BE CAREFUL THAT THIS FOLLOWS norm() SYNTAX
    ord <- param[3]
    gap <- 1
  }

  if (t1 > tk) {
    tk_temp <- tk
    tk <- t1
    t1 <- tk_temp
  }

  V_row <- rep(0,N)
  Vp_row <- V_row #hopefully does not point to it like python

  t_grid <- t[seq(t1,(tk+1),gap)]
  dts <- diff(t_grid) #time steps
  w <- 0.5*(append(dts,0) + append(0, dts)) #appends 0 to dts and dts to 0

  V_row[, seq(t1,(tk+1),gap)] = g(t_grid, t[t1], t[tk])*w #may need to be t1-1, tk-1
  Vp_row[, seq(t1,(tk+1),gap)] = -gp(t_grid, t[t1], t[tk])*w
  Vp_row[,t1] = Vp_row[,t1] - g(t[t1], t[t1], t[tk])
  Vp_row[,tk] = Vp_row[,tk] + g(t[tk], t[t1], t[tk])

  if (pow != 0) {
    if (ord == 0 ){
      scale_fac <- norm(as.vector(V_row[,seq(t1,(tk+1),gap)]),
                       nrm)
    } else if (ord == 1) {
      scale_fac <- norm(as.vector(Vp_row[,seq(t1,(tk+1),gap)]),
                        nrm)
    } else {
      scale_fac <- mean(dts)
    }
  } else {
    # do nothing
  }

  Vp_row <- Vp_row/scale_fac
  V_row <- V_row/scale_fac

  #now get everything into compiable state
  anslist <- list('Vp_row' <- Vp_row,
                  'V_row' <- V_row)
  return(anslist)

}
