# basis_fcn.R

### INPUTS...
# p:
# q:

# Need to figure out how to specify (t, t1, tk)

basis_fcn <- function(p, q) {
  g <- function(t, t1, tk) {
    (p > 0)*(q > 0)*(t - t1)^max(p, 0)*(tk - t)^max(q, 0) +
      (p == 0)*(q == 0)*(1 - 2 * abs(t - (t1+tk)/2)/(tk - t1)) +
      (p > 0)*(q < 0)*sin(p*pi/(tk - t1)*(t - t1)) +
      (p == -1)*(q == -1)
  }

  gp <- function(t, t1, tk) {
    (t-t1)^(max(p-1, 0))*(tk-t)^(max(q-1, 0))*((-p-q)*t+p*tk+q*t1)*(q > 0)*(p > 0) +
      -2*sign(t - (t1+tk)/2)/(tk-t1)*(q == 0)*(p == 0) +
      p*pi/(tk-t1)*cos(p*pi/(tk-t1)*(t-t1))*(q < 0)*(p > 0) +
      0*(p == -1)*(q == -1)
  }

  if (p > 0 & q > 0) {
    normalize <- function(t, t1, tk) {
      (t - t1)^max(p, 0)*(tk - t)^max(q, 0)
    }

    g <- function(t, t1, tk) {
      ((p > 0)*(q > 0)*(t - t1)^max(p, 0)*(tk - t)^max(q, 0) +
         (p == 0)*(q == 0)*(1 - 2*abs(t - (t1+tk)/2) / (tk - t1)) +
         (p > 0)*(q < 0)*sin(p*pi/(tk - t1)*(t - t1)) +
         (p == -1)*(q == -1))/(abs(normalize((q*t1+p*tk)/(p+q), t1, tk)))
    }

    gp <- function(t, t1, tk) {
      ((t-t1)^(max(p-1, 0))*(tk-t)^(max(q-1, 0))*((-p-q)*t+p*tk+q*t1)*(q > 0)*(p > 0) +
         -2*sign(t-(t1+tk)/2)/(tk-t1)*(q == 0) * (p == 0) +
         p*pi/(tk-t1)*cos(p*pi/(tk-t1)*(t-t1))*(q < 0)*(p > 0) +
         0*(p == -1)*(q == -1))/(abs(normalize((q*t1+p*tk)/(p+q), t1, tk)))
    }
  }

  anslist <- list("g" = g,
                  "gp" = gp)
  return(anslist)
}
