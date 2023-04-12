# Adaptive_Grid.R

### INPUTS...
# t:
# xobs:
# params:

install.packages("Matrix")
install.packages('pracma')
library(Matrix) #to be able to run sparse diags
library(pracma) #to be able to run repmat and linspace

Adaptive_Grid <- function(t, xobs, params = NULL) {

  # function code here
  if (is.null(params) == TRUE) {
    index_gap <- 16
    K <- max(as.integer(floor(length(t))/50), 4)
    p <- 2
    tau <- 1
  } else {
    index_gap <- params[1]
    K <- params[2]
    p <- params[3]
    tau <- params[4]
  }

  M <- length(t)

  gs <- basis_fcn(p,p) #Need to create function
  g <- gs$g
  gp <- gs$gp

  ags <- AG_tf_mat_row(g, gp, t, 1, 1+index_gap, c(1,1,0))
  o <- ags$o
  Vp_row <- ags$Vp_row

  #Below lines may need work *****************
  Vp_diags <- repmat(Vp_row[,1:(index_gap+1)], (M-index_gap), 1)
  Vp <- spDiags(T(Vp_diags), seq(0,(index_gap+1)), (M-index_gap), M) #create sparse mat

  weak_der <- Vp %*% xobs
  weak_der <- append(matrix(0, as.integer(floor(index_gap/2)), 1), weak_der)
  weak_der <- append(weak_der, matrix(0, as.integer(floor(index_gap/2)), 1))

  Y <- abs(weak_der)
  Y <- cumsum(Y) #gets cumulative sum
  Y <- Y/Y[length(Y)] #divide by last element in Y

  Y <- tau*Y + (1-tau)*t(linspace(Y[1], Y[length(Y)], length(Y)))

  temp1 <- Y[as.integer(floor(index_gap/2)) - 1]
  temp2 <- Y[as.integer(length(Y) - ceil(index_gap/2)) - 1]
  U <- linspace(temp1, temp2, (K+2))

  final_grid <- matrix(0, nrow = 1, ncol = K)

  for (i in 1:K) {
    final_grid[1, i] <- which((Y-U[i+1]) >= 0)[1] #could also use min(which())
  }

  final_grid <- unique(final_grid) #keep only unique values

  return(final_grid)


}
