# getWsindyAdaptive.R

### INPUTS...
# xobs: x values
# tobs: t values
# r_whm: r width half max
# s: test function support
# K: # of test function
# p: test function degree
# tau_p: test function has value 10^-tau_p at penultimate support point. Or, if tau_p < 0, directly sets poly degree p = -tau_p

getWsindyAdaptive <- function(xobs, tobs, r_whm = 30, s = 16, K = 120, p = 2, tau_p = 16) {

  # function code here
  tau <- 1

  if (wsinit@mult_trajectories == "True") {
    x_values <- xobs[1]
    t_values <- tobs[1]

    for (i in 2:length(xobs)) {
      x_values <- cbind(x_values, xobs[i])
      t_values <- rbind(t_values, tobs[i])
    }
    xobs <- x_values
    tobs <- t_values
  } else {
    #do nothing
  }

  wsindy_params <- c(s, K, p, tau)

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

  for (i in 1:n) {

    dgrid <- Adaptive_Grid(tobs, xobs[,i], wsindy_params) #call ad grid fcn
    grid_i <- dgrid$final_grid #get the returned object from adaptive grid

    buildadap <- VVp_build_adaptive_whm(tobs, grid_i, r_whm, c(0,Inf,0))

    #get the answers from function
    V <- buildadap$V
    Vp <- buildadap$Vp
    ab_grid <- buildadap$ab_grid

    mats <- rbind(mats, rbind(V, Vp)) #may need work
    ts_grids <- rbind(ts_grids, ab_grid) #^same

    if (wsinit@useGLS > 0) {
      Cov <- (Vp %*% t(Vp)) + wsinit@useGLS*diag(dim(V)[1])
      RT <- linalg_cholesky(Cov)
      Gmod <- lm(formula = RT ~ V%*%Theta_0)
      G <- Gmod$coefficients[2] #gets slope from this (python and R output opposite)
      bmod <- lm(formula = RT ~ Vp%*%xobs[,i])
      b <- bmod$coefficients[2]

    } else {
      RT <- 1/norm(Vp, type = "2")
      RT <- reshape(RT, length(RT), 1) #turn into column vector **
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


}
