#lorenz.R

#Creating the Lorenz function:

lorenz <- function(x, sigma, beta, rho) {
  a <- sigm*(x[2]-x[1])
  b <- x[1]*(rho-x[3]-x[1])
  c <- x[1]*x[2] - beta*x[3]

  abc <- c(a,b,c)

  return(abc) #return the array of solutions
}
