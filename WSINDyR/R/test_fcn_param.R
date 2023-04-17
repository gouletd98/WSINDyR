# test_fcn_param.R

### INPUTS...
# r:
# c:
# t:
# p:

# "tau_p" comes from 'getWsindyAdaptive.R', not "wsinit"
# circle back to this!!!

test_fcn_param <- function(r, c, t, p = NULL) {

  if (wsinit@tau_p < 0) {
    wsinit@tau_p <- -wsinit@tau_p
  } else {
    p <- wsinit@tau_p
    wsinit@tau_p <- 16
  }

  dt <- t[2] - t[1]
  r_whm <- r * dt

  A <- log2(10) * wsinit@tau_p

  gg <- function(s) {
    return (-s^2 * ((1 - (r_whm/s)^2)^A - 1))
  }

  hh <- function(s) {
    return ((s - dt)^2)
  }

  ff <- function(s) {
    return (hh(s) - gg(s))
  }

  s <- uniroot(ff, interval=c(r_whm, r_whm * sqrt(A) + dt))$root

  if (is.null(p)) {
    p <- min(ceiling(max(-1/log2(1 - (r_whm/s)^2), 1)), 200)
  }

  a <- which(t >= (c - s))[1]

  if (c + s > tail(t, 1)) {
    b <- length(t)
  } else {
    b <- which(t >= (c + s))[1]
  }

  anslist <- list("p" = p,
                  "a" = a,
                  "b" = b)
  return(anslist)
}
