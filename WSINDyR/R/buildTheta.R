# buildTheta.R

### INPUTS...
# xobs:

buildTheta <- function(xobs) {

  ans_poolDatagen <- poolDatagen(xobs)
  theta_0 <- ans_poolDatagen$theta_0
  tags <- ans_poolDatagen$tags

  if (wsinit@scaled_theta > 0) {
    # M_diag <- apply(theta_0, 2, function(col) {
    #   norm(col, wsinit@scale_theta)
    #
    # })
    M_diag <- norm(theta_0*wsinit@scaled_theta, "I") #Compute the scaled infinity norm

    M_diag <- matrix(M_diag, nrow = length(M_diag), ncol = 1)

    anslist <- list("theta_0" = theta_0,
                    "tags" = tags,
                    "M_diag" = M_diag)
    return(anslist)
  } else {
    M_diag <- array(dim = c(0, 0))

    anslist <- list("theta_0" = theta_0,
                    "tags" = tags,
                    "M_diag" = M_diag)
    return(anslist)
  }
}
