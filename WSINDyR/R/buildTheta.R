# buildTheta.R

### INPUTS...
# xobs:

# NOTE - need to loop back and check discrepancies with "self"

buildTheta <- function(xobs) {

  anspoolDatagen <- poolDatagen(xobs)
  theta_0 <- anspoolDatagen$theta_0
  tags <- anspoolDatagen$tags

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
