# buildTheta.R

### INPUTS...
# xobs:

# NOTE - need to loop back and check discrepancies with "self"

buildTheta <- function(self, xobs) {
  poolDatagen <- self$poolDatagen(xobs)
  theta_0 <- poolDatagen[[1]]
  tags <- poolDatagen[[2]]

  if (self$scale_theta > 0) {
    M_diag <- apply(theta_0, 2, function(col) {
      norm(col, self$scale_theta)
    })
    M_diag <- matrix(M_diag, ncol = 1)
    return(list(theta_0, tags, M_diag))
  } else {
    M_diag <- matrix(nrow = 0, ncol = 0)
    return(list(theta_0, tags, M_diag))
  }
}
