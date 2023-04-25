# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

x <- as.vector(1, 3, 5, 7, 9)
y <- as.vector(2, 4, 6, 8, 10)
z <- 5

test <- function(x, y, z) {
  result <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {

    if (y[i] < z) {
      result[i] <- x[i] + y[i]

    } else {
      result[i] <- x[i] - y[i]

    }
  }
  return(result)
}

### lstsq() example

### qr.solve() example - sparsifyDynamics.R
