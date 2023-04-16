# buildTheta.R

### INPUTS...
# xobs:

# NOTE - need to loop back and check discrepancies with "self"

buildTheta <- function(xobs) {

  anspoolDatagen <- poolDatagen(xobs)
  theta_0 <- anspoolDatagen$theta_0
  tags <- anspoolDatagen$tags

  if (wsinit@scaled_theta > 0) {
    M_diag <- apply(theta_0, 2, function(col) {
      norm(col, wsinit@scale_theta)
    })
    M_diag <- matrix(M_diag, ncol = 1)
    anslist <- list("theta_0" = theta_0,
                    "tags" = tags,
                    "M_diag" = M_diag)
    return(anslist)
  } else {
    M_diag <- matrix(nrow = 0, ncol = 0)
    anslist <- list("theta_0" = theta_0,
                    "tags" = tags,
                    "M_diag" = M_diag)
    return(anslist)
  }
}
