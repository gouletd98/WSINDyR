#simODE - gets ode simulations

simODE <- function(x0, t_span, t_eval, tol_ode, ode_name, params,
                   noise_ratio) {

  if (ode_name == 'Linear') {
    A <- params[1,]

    rhs <- function(t,x) {
      return(A%*%x) #return a dot x
    }

    #weights <- matrix(NULL, nrow = 2, ncol = 2) #initialize the weights, but understand this could be wrong.
    for (i in 1:as.numeric(dim(A)[1])) {
      weights <- cbind(diag(dim(A)[1]), A[,i])
    }


  } else if (ode_name == 'Logistic_Growth') {
    pow <- 2
    rhs <- function(t,x) {
      return(x - x^pow)
    }

    weights <- list(matrix(c(1, 1, pow, -1), nrow=2, byrow=TRUE))

  } else if (ode_name == 'Van_der_Pol') {
    mu <- params[1]
    rhs <- function(t,x) {
      return(c(x[2], mu*x[2]-mu*x[1]^2*x[2]-x[1]))
    }



  } else if (ode_name == 'Duffing') {
    mu <- 0.2
    ode_params <- c(mu, mu^2/(4*5), 1)
    x0 <- t(c(0,2)) #np.array([3,0]).T
    t_span <- c(0, 30)
    t_eval <- seq(0, 30, 0.01)

  } else if (ode_name == 'Lotka_Volterra') {
    alpha <- 2/3
    beta <- 4/3
    ode_params <- c(alpha, beta, 1, 1)
    x0 <- t(c(10,10)) #np.array([3,0]).T
    t_span <- c(0, 200)
    t_eval <- seq(0, 200, 0.02)

  } else if (ode_name == 'Lorenz') {
    ode_params <- c(10, 8/3, 27)
    x0 <- t(c(-8, 10, 27)) #np.array([3,0]).T
    t_span <- c(0.001, 10)
    t_eval <- linspace(0.001, 10, 5000)

  } else {
    disp('No ODE selected')
  }
}
