# WSINDy Example in R
# Last Revised: 04/24/23

# Import Libraries --------------------------------------------------------

#install.packages("pracma")
library(pracma) #for linspace
library(torch)
library(deSolve)
library(combinat)
#also add the path with all functions

# rm(list = setdiff(ls(), lsf.str())) #removes all things but functions

# Determine ODE problem ---------------------------------------------------

ode_num <- 5 # select ODE system from the list ode_names (1-6)
tol_ode <- 1e-15                    # sol_ivp tolerance (abs and rel) for generating data
noise_ratio <- 0.05  #set signal-to-noise ratio(L2 sense)
set.seed(42)
#np.random.seed(42)                 #reproducibility
useFD_SINDy <- 1                    #SINDy finite difference differentiation order, if 0 uses TVdiff
use_preset_params <- "True"          #Use parameters specified in gen_data

#ode params
ode_names = c('Linear','Logistic_Growth',
              'Van_der_Pol','Duffing','Lotka_Volterra',
              'Lorenz', 'SIR')
ode_name = ode_names[ode_num]

if (ode_name == 'Linear') {
  ode_params <- matrix(c(-0.1,-2,2,-0.1),nrow = 2, ncol=2) #np.array([[[-0.1, 2], [-2, -0.1]]])
  x0 <- as.matrix(c(3,0), nrow = 2, ncol = 1) #np.array([3,0]).T
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
  t_eval <- linspace(0.001, 10, 5000) #last tried 15000

} else if (ode_name == 'SIR') {
  ode_params <- c(0.4, 0.04) #beta, gamma
  x0 <- t(c(997, 3, 0)) #S(0), I(0), R(0)
  t_span <- c(0,100)
  t_eval <- seq(0,100, 0.01)

} else {
  disp('No ODE selected')
}

# make sure to run simODE.R
z = simODE(x0, t_span, t_eval, tol_ode, ode_name, ode_params, noise_ratio)
weights <- z$weights
t <- z$sol[,1]
xobs <- as.matrix(z$xobs)
rhs <- z$rhs

# plot(t, xobs)
par(mfrow=c(1,1))
plot(t, xobs[,1], col = "blue", pch = 18) #, ylim = c(-30,50))
points(t, xobs[,2], col = 'orange', pch = 16)

#if Lorenz
points(t, xobs[,3], col = "green", pch = 16, cex = 0.7)


# Build WSINDy model  -----------------------------------------------------

tobs <- t
param <- ode_params

#If polys need to be changed:
wsinit@polys <- seq(0,3,1)
wsinit@ld <- 0.1
wsinit@scaled_theta <- 0

wsind <- getWSindyUniform(xobs, t, L = 35, overlap = 0.7)

#FOR LINEAR
#wsindsim <- simulate(x0 = x0, t_span = seq(0,30,1), t_eval = seq(0,30,.001))

#FOR Lorenz
wsindsim <- simulate(x0 = x0, t_span, t_eval)

tws <- wsindsim[,1]
xgs1 <- wsindsim[,2]
xgs2 <- wsindsim[,3]
par(mfrow=c(2,1))
plot(t, xobs[,1], col = "blue", pch = 18) #, xlim = c(0,30))
lines(tws, xgs1, col = "red", lty = 2, lwd = 2)

plot(t, xobs[,2], col = 'purple', pch = 16) #, xlim = c(0,30))
lines(tws, xgs2, col = 'black', lty = 2, lwd = 2)


#If system has 3 variables
xgs3 <- wsindsim[,4]
points(t, xobs[,3], col = "green", pch = 16, cex = 0.7)
lines(tws, xgs2, col = 'black', lty = 2, lwd = 2)
