# buildTheta.R

### INPUTS...
# xobs:

# NOTE - need to loop back and check discrepancies with "self"

buildTheta <- function(xobs) {

  anspoolDatagen <- poolDatagen(xobs)
  theta_0 <- anspoolDatagen$theta_0
  tags <- anspoolDatagen$tags

  if (wsinit@scale_theta > 0) {
    M_diag <- apply(theta_0, 2, function(col) {
      norm(col, wsinit@scale_theta)
    })
    M_diag <- matrix(M_diag, ncol = 1)
    return(list(theta_0, tags, M_diag))
  } else {
    M_diag <- matrix(nrow = 0, ncol = 0)
    return(list(theta_0, tags, M_diag))
  }
}
