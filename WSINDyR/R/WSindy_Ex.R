#WSindy Example in R
#last Revised - 4/5/23

# Import Libraries --------------------------------------------------------

install.packages("pracma")
library(pracma)

#also add the path with all functions


# Determine ODE problem ---------------------------------------------------

ode_num <- 1 # select ODE system from the list ode_names (1-6)
tol_ode <- 1e-15                    # sol_ivp tolerance (abs and rel) for generating data
noise_ratio <- 0.1  #set signal-to-noise ratio(L2 sense)
set.seed(42)
#np.random.seed(42)                 #reproducibility
useFD_SINDy <- 1                    #SINDy finite difference differentiation order, if 0 uses TVdiff
use_preset_params <- "True"          #Use parameters specified in gen_data

#ode params
ode_names = c('Linear','Logistic_Growth',
              'Van_der_Pol','Duffing','Lotka_Volterra',
              'Lorenz')
ode_name = ode_names[ode_num]

if (ode_name == 'Linear') {
  ode_params <- matrix(c(-0.1,-2,2,-0.1),nrow = 2, ncol=2) #np.array([[[-0.1, 2], [-2, -0.1]]])
  x0 <- t(c(3,0)) #np.array([3,0]).T
  t_span <- c(0, 15)
  t_eval <- linspace(0, 15, 1501)

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

#NEED to input some of the example code here

#run initialization function
#WSINDy_model = initialize.wsindy(polys <- seq(0,5,1))

#WSINDy_model <- getWSindyUniform
