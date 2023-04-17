# simulate.R

### INPUTS...
# x0: the initial guess
# t_span: total time
# t_eval: times to evaluate at

# get all packages
install.packages('deSolve')
library(deSolve) # this allows us to solve IVPS

# now get into function

simulate <- function(x0, t_span, t_eval) {

  rows <- as.numeric(dim(wsind$tags)[1])
  cols <- as.numeric(dim(wsind$tags)[2])

  tol_ode <- 10e-14 #tolerance for our ode

  #create a RHS function
  rhs <- function(t, x) {
    term <- rep(1,rows) #create vectors of one
    for (row in 1:rows) {
      for (col in 1:cols) {
        term[row] <- term[row]*x[col]^(wsind$tags[row,col])
      }
    }

    return (term %*% wsind$coef) #returns rhs dotted with coef from uniform
  }

  # the example must start to be run before the following line
  sol <- ode(y = x0, times = t_eval, func = rhs, parms = ode_params) # UNSURE what to call for parameters
  return(t(sol[,"Y"])) # returns transpose of y solution
}
