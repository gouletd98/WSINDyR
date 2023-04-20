# simODE.R - gets ODE simulations


install.packages('deSolve')
library(deSolve) # this allows us to solve IVPS

simODE <- function(x0, t_span, t_eval, tol_ode, ode_name, params,
                   noise_ratio) {

  if (ode_name == 'Linear') {
    A <- params

    rhs <- function(t,x0, params) {
      with(as.list(c(x0,params)), {
        dx <- A%*%x0
        list(c(dx))
      })
    }
    for (i in 1:as.numeric(dim(A)[1])) {
      weights <- cbind(diag(dim(A)[1]), A[i,])
    }



  } else if (ode_name == 'Logistic_Growth') {

    pow <- 2
    rhs <- function(t,x0, params, pow) {
      with(as.list(c(x0,params)), {
        dx <- x0 - x0^params[1]
        list(dx)
      })
    }

    weights <- list(matrix(c(1, 1, pow, -1), nrow=2, byrow=TRUE))

  } else if (ode_name == 'Van_der_Pol') {
    mu <- params[1]
    rhs <- function(t,x) {
      return(c(x[2], mu*x[2]-mu*x[1]^2*x[2]-x[1]))
    }

    datas <- c(0,1,1, 1, 0, -1, 0, 1, mu, 2, 1, -mu)
    weights = matrix(datas, nrow = 4, ncol = 3, byrow = TRUE)

  w} else if (ode_name == 'Duffing') {

    mu <- params[1]
    alpha <- params[2]
    beta <- params[3]

    rhs <- function(t,x) {
      return(c(x[2], -mu*x[2]-alpha*x[1]-beta*x[1]^3))
    }

    datas <- c(0,1,1, 1, 0, -alpha, 0, 1, -mu, 3, 0, -beta)
    weights = matrix(datas, nrow = 4, ncol = 3, byrow = TRUE)


  } else if (ode_name == 'Lotka_Volterra') {

    alpha <- params[1]
    beta <- params[2]
    delta <- params[3]
    gamma <- params[4]

    rhs <- function(t,x) {
      reutrn(c(alpha*x[1]-beta*x[1]*x[2], delta*x[1]*x[2] - gamma*x[2]))
    }

    datas <- c(1, 0, alpha, 1, 1, -beta, 0, 1, -gamma, 1, 1, delta)
    weights <- matrix(datas, nrow = 4, ncol = 4, byrow = TRUE)

  } else if (ode_name == 'Lorenz') {

    sigma <- params[1]
    beta <- params[2]
    rho <- params[3]

    # rhs <- function(t,x) {
    #   return(lorenz(x, sigma, beta, rho))
    # }

    rhs <- function(t, x0, params) {
      with(as.list(c(x0, params)), {
        dX <-  params[1] * (x0[2] - x0[1])
        dY <-  x0[1]*(params[3]-x0[3]) - x0[2]
        dZ <- x0[1]*x0[2] - params[2]*x0[3]
        list(c(dX, dY, dZ))
      })
    }

    datas <- c(0, 1, 0, sigma,
               1, 0, 0, -sigma,
               1, 0, 0, rho,
               1, 0, 1, -1,
               0, 1, 0, -1,
               1, 1, 0, 1,
               0, 0, 1, -beta)

    weights <- matrix(datas, nrow= 7, ncol = 4, byrow = TRUE)

  } else {
    disp('No ODE selected')
  }

  sol <- ode(y = x0, times = t_eval, func = rhs, parms = ode_params)

  x <- sol[,2:dim(sol)[2]]
  xobs <- addNoise(x, noise_ratio)

  anslist <- list('weights' = weights,
                  'sol' = sol,
                  'xobs' = xobs,
                  'rhs' = rhs)

  return(anslist)
}

lorenz <- function(x, sigma, beta, rho) {
  a <- sigm*(x[2]-x[1])
  b <- x[1]*(rho-x[3]-x[1])
  c <- x[1]*x[2] - beta*x[3]

  abc <- c(a,b,c)

  return(abc) # return the array of solutions
}
