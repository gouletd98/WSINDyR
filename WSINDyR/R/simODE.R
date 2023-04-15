#simODE - gets ode simulations

simODE <- function(x0, t_span, t_eval, tol_ode, ode_name, params,
                   noise_ratio) {

  if (ode_name == 'Linear') {
    A <- params[1]

    rhs <- function(t,x) {
      return(A%*%x) #return a dot x
    }

    weights <- list() #initialize the weights, but understand this could be wrong.
    for (i in 1:ncol(A)) {
      weights[[i]] <- cbind(diag(ncol(A)), A[,i])
    }

    #LEFT OFF RIGHT HEREEEEEEEE!!

  } else if (ode_name == 'Logistic_Growth') {
    ode_params <- c(2)
    x0 <- t(c(.01)) #np.array([3,0]).T
    t_span <- c(0, 15)
    t_eval <- seq(0, 10, 0.005)

  } else if (ode_name == 'Van_der_Pol') {
    dt <- 0.01
    ode_params <- c(4)
    x0 <- t(c(0,1)) #np.array([3,0]).T
    t_span <- c(0, 30)
    t_eval <- seq(0, 30, dt)

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
