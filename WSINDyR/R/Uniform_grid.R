# Uniform_grid.R

### INPUTS...
# t:
# L:
# s:
# param:

Uniform_grid <- function(t, L, s, param) {

  M <- length(t)
  p <- 16

  overlap <- as.integer(floor(L*(1-sqrt(1-s^(1/p)))))

  grid <- c()
  a <- 0
  b <- L

  grid <- append(grid, c(a,b)) #append a and b to grid

  #make it a matrix now
  grid <- matrix(grid, nrow = 1, ncol = 2)

  while ((b-overlap+L) <= (M-1)) {
    a <- b - overlap
    b <- a + L
    grid <- rbind(grid, c(a,b))

  }

  N <- dim(grid)[1] #get the amount of rows to go through

  V <- matrix(0, nrow = N, ncol = M)
  Vp <- matrix(0, nrow = N, ncol = M)

  for (k in 1:N) {
    gs <- basis_fcn(p,p)
    g <- gs$g
    gp <- gs$gp

    a <- grid[k,1]
    b <- grid[k,2]

    Vs <- tf_mat_row(g, gp, t, a, b, param)
    V_row <- Vs$V_row
    Vp_row <- Vs$Vp_row

    V[k,] <- V_row
    Vp[k,] <- Vp_row

  }

  anslist <- list('V' = V,
                  'Vp' = Vp,
                  'grid' = grid)
  return(anslist)
}
