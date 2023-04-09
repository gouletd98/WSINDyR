#getWSindyUniform

#get packages first
install.packages("torch")
library(torch) #used for cholesky decomp

#NOTE:
#To call this function, please call the output as wsind **
#otherwise, the link to other functions will be broken

#INPUTS:
#xobs - x values of observations
#tobs - T values of time
#L - test function support
#overlap - overlap ratio

#initalize parameters
L <- 30
overlap <- 0.5

getWSindyUniform <- function(xobs, tobs, L, overlap) {

  if (wsinit@mult_trajectories == "True") {
    x_values <- xobs[1]
    t_values <- tobs[1]

    for (i in 2:length(obs)) {
      x_values <- cbind(x_values, xobs[i])
      t_values <- rbind(t_values, tobs[i])
    }
    xobs <- x_values
    tobs <- t_values
  } else {
    #do nothing
  }

  M <- length(tobs)
  thetbuild <- buildTheta(xobs) #this will need to be created ********
  #with the above function created we can return the list here and call to it
  Theta_0 <- thetbuild$theta_0
  tags <- thetbuild$tags
  M_diag <- thetbuild$M_diag

  #following will only work on matrices
  n <- dim(xobs)[2] #gets the columns in xobs
  w_sparse <- matrix(0, dim(Theta_0)[2], n) #create sparse matrix

  #**** Might need to change below based off initialization and appending
  mats <- c() #empty vector
  ts_grids <- c() #empty vector

  ugrid <- Uniform_grid(tobs, L, overlap, c(0, Inf, 0)) #This will need to be created****

  #with above function we can return list and call to it
  V <- ugrid$V
  Vp <- ugrid$Vp
  grid <- ugrid$grid

  for (i in 1:n) {

    # mats <- append(mats, c(V, Vp), after = length(mats)) #may need work
    # ts_grids <- append(ts_grids, grid, after = length(ts_grids)) #^same
    mats <- rbind(mats, rbind(V, Vp)) #may need work
    ts_grids <- rbind(ts_grids, grid) #^same

    if (wsinit@useGLS > 0) {
      Cov <- (Vp %*% t(Vp)) + wsinit@useGLS*diag(dim(V)[1])
      RT <- linalg_cholesky(Cov)
      Gmod <- lm(formula = RT ~ V%*%Theta_0)
      G <- Gmod$coefficients[2] #gets slope from this (python and R output opposite)
      bmod <- lm(formula = RT ~ Vp%*%xobs[,i])
      b <- bmod$coefficients[2]

    } else {
      RT <- 1/norm(Vp, type = "2")
      RT <- reshape(RT, length(RT), 1) #turn into column vector
      G <- (V%*%Theta_0)*RT
      temp <- Vp%*%xobs[,i]
      b <- t(RT)*temp
    }

    if (wsinit@scaled_theta > 0) {
      w_sparse_temp <- sparsifyDynamics((G*(1/T(M_diag))), b, 1) #*** NEEED to make function
      temptemp <- as.vector((1/M_diag)*w_sparse_temp)
      w_sparse[,i] = temptemp
    } else {
      w_sparse_temp <- sparsifyDynamics(G,b,1)
      w_sparse[,i] <- as.vector(w_sparse_temp)
    }


  }

  #now get everything into compiable state
  anslist <- list('coef' <- w_sparse,
                  'tags' <- tags,
                  'mats' <- mats,
                  'ts_grids' <- ts_grids)
  return(anslist)

  #MUST now call function with an output associated to get list.


}
