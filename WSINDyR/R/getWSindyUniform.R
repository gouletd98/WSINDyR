# getWSindyUniform.R

### INPUTS...
# xobs: x values of observations
# tobs: T values of time
# L: test function support
# overlap: overlap ratio

#NOTE:
#To call this function, please call the output as wsind **
#otherwise, the link to other functions will be broken

getWSindyUniform <- function(xobs, tobs, L, overlap) {

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

  M <- length(tobs)
  thetbuild <- buildTheta(xobs)

  Theta_0 <- thetbuild$theta_0
  tags <- thetbuild$tags
  M_diag <- thetbuild$M_diag

  #following will only work on matrices
  n <- dim(xobs)[2] #gets the columns in xobs
  w_sparse <- matrix(0, nrow = dim(Theta_0)[2], ncol = n) #create sparse matrix

  #initializes lists of tsgrids and mats
  mats <- list()
  ts_grids <- list()

  ugrid <- Uniform_grid(tobs, L, overlap, param = c(0, "I", 0))

  #with above function we can return list and call to it
  V <- ugrid$V
  Vp <- ugrid$Vp
  grid <- ugrid$grid

  for (i in 1:n) {

    mats <- c(mats, list(list(V,Vp)))
    ts_grids <- c(ts_grids, list(grid))

    if (wsinit@useGLS > 0) {
      Cov <- (Vp %*% t(Vp)) + wsinit@useGLS*diag(dim(V)[1])
      # RT <- linalg_cholesky(Cov)
      RT <- chol(Cov) #cholesky decomp on covariance

      #G <- solve(as.matrix(RT), as.matrix(V)%*%as.matrix(Theta_0), tol = 1e-15)

      A <- as.matrix(RT)
      B <- as.matrix(V)%*%as.matrix(Theta_0)

      G <- solve(t(A) %*% A) %*% t(A) %*% B

      #CURRENT ONE
      # b <- solve(as.matrix(RT), Vp%*%xobs[,i], tol = 1e-15)

      B2 <- Vp%*%xobs[,i]

      b <- solve(t(A) %*% A) %*% t(A) %*% B2

    } else {
      RT <- 1/norm(Vp, type = "2")
      RT <- reshape(RT, length(RT), 1) #turn into column vector
      G <- (V%*%Theta_0)*RT
      temp <- Vp%*%xobs[,i]
      b <- t(RT)*temp
    }

    if (wsinit@scaled_theta > 0) {
      w_sparse_temp <- sparsifyDynamics((G%*%(1/(M_diag))), b, 1, NULL)
      temptemp <- as.vector((1/M_diag)%*%as.matrix(w_sparse_temp$Xi))
      w_sparse[,i] = temptemp
    } else {
      w_sparse_temp <- sparsifyDynamics(G,b,1, NULL)
      w_sparse[,i] <- as.vector(w_sparse_temp$Xi)
    }


  }

  # now get everything into compiled state
  anslist <- list('coef' = w_sparse,
                  'tags' = tags,
                  'mats' = mats,
                  'ts_grids' = ts_grids)
  return(anslist)

}
