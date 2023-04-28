# WSINDy Example in R
# Last Revised: 04/28/23

# Import Libraries --------------------------------------------------------

#install.packages("pracma") #example of installing packages
library(pracma) #for linspace
library(deSolve)
library(combinat)

# rm(list = setdiff(ls(), lsf.str())) #removes all things but functions

# Determine ODE problem ---------------------------------------------------

ode_num <- 4 # select ODE system from the list ode_names (1-6)
tol_ode <- 1e-15          # sol_ivp tolerance (abs and rel) for generating data
noise_ratio <- 0.05  #set signal-to-noise ratio(L2 sense)
set.seed(42)
useFD_SINDy <- 1  #SINDy finite difference differentiation order, if 0 uses TVdiff
use_preset_params <- "True"     #Use parameters specified in gen_data

#ode params
ode_names = c('Linear','Logistic_Growth',
              'Van_der_Pol','Duffing','Lotka_Volterra',
              'Lorenz')
ode_name = ode_names[ode_num]

if (ode_name == 'Linear') {
  ode_params <- matrix(c(-0.1,-2,2,-0.1),nrow = 2, ncol=2) #np.array([[[-0.1, 2], [-2, -0.1]]])
  x0 <- as.matrix(c(3,0), nrow = 2, ncol = 1) #np.array([3,0]).T
  t_span <- c(0, 15)
  t_eval <- linspace(0, 15, 1501)

} else if (ode_name == 'Logistic_Growth') {
  ode_params <- c(2)
  x0 <- t(c(.01))
  t_span <- c(0, 15)
  t_eval <- seq(0, 10, 0.005)

} else if (ode_name == 'Van_der_Pol') {
  dt <- 0.01
  ode_params <- c(4)
  x0 <- t(c(0,1))
  t_span <- c(0, 30)
  t_eval <- seq(0, 30, dt)

} else if (ode_name == 'Duffing') {
  mu <- 0.2
  ode_params <- c(mu, mu^2/(4*5), 1)
  x0 <- t(c(0,2))
  t_span <- c(0, 30)
  t_eval <- seq(0, 30, 0.01)

} else if (ode_name == 'Lotka_Volterra') {
  alpha <- 2/3
  beta <- 4/3
  ode_params <- c(alpha, beta, 1, 1)
  x0 <- t(c(10,10))
  t_span <- c(0, 100)
  t_eval <- seq(0, 100, 0.01)

} else if (ode_name == 'Lorenz') {
  ode_params <- c(10, 8/3, 27)
  x0 <- t(c(-8, 10, 27))
  t_span <- c(0.001, 10)
  t_eval <- linspace(0.001, 10, 5000)

} else {
  disp('No ODE selected')
}

#Simulate the ODE
z = simODE(x0, t_span, t_eval, tol_ode, ode_name, ode_params, noise_ratio)
weights <- z$weights
t <- z$sol[,1]
xobs <- as.matrix(z$xobs)
rhs <- z$rhs

#Plot results to verify
colors = c("blue", "purple", "red")
col2s = c("black", "black", "black")
par(mfrow=c(dim(xobs)[2],1))
for (i in 1:dim(xobs)[2]) {
  plot(t, xobs[,i], col = colors[i], pch = 16)
}

# Build WSINDy model  -----------------------------------------------------

tobs <- t
param <- ode_params

#If polys need to be changed:
#NOTE - the following 4 values can be adjusted based off ODE selection
wsinit@polys <- seq(0,3,1)
wsinit@ld <- 0.01
# wsinit@scaled_theta <- 0
# wsinit@gamma <- 0.05

wsind <- getWSindyUniform(xobs, t, L = 30, overlap = 0.7)

#FOR Lorenz
wsindsim <- simulate(x0 = x0, t_span, t_eval)

#plotting ODE vs. WSINDy
par(mfrow=c(dim(xobs)[2],1))
for (i in 1:dim(xobs)[2]) {
  plot(t, xobs[,i], col = colors[i], pch = 16)
  lines(wsindsim[,1], wsindsim[,i+1], col = col2s[i], lty = 2, lwd = 2)
}


#now plot test function locations
par(mfrow=c(dim(xobs)[2],1))
for (i in 1:dim(xobs)[2]) {
  plot(t, xobs[,i], col = colors[i], pch = 16)
  temp <- floor(apply(wsind$ts_grids[[i]], 1, mean))
  plpoints <- mean(xobs[,i])*as.vector(matrix(1, nrow = dim(wsind$ts_grids[[i]])[1], ncol = 1))
  points(t[temp], plpoints, col = col2s[i], pch = 1)
}
