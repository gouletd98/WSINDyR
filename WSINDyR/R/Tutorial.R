### Tutorial.R (APPM 4720/5720)
### Adam Manaster and Derek Goulet
### Last Edit on 05/02/2023

# Basic Function(s) -------------------------------------------------------

# define function `test` below

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

# intialize variables - concatenating `x0` and `y0` into vectors
x0 <- c(1, 3, 5, 7, 9)
y0 <- c(2, 4, 6, 8, 10)
z0 <- 5

# run the function and view result
test(x = x0, y = y0, z = z0)

### now, trying out some of the `as.<DATA>()` functions

# create nested list and turn into matrix
my_list <- list(list(1, 2, 3, 4, 5), list(6, 7, 8, 9, 10))
my_matrix <- as.matrix(my_list)

class(my_list)
class(my_matrix)

# create numeric and turn into character
my_num <- 100
my_char <- as.character(my_num)

class(my_num)
class(my_char)

### also, showcasing the functionality of `rbind()` and `cbind()`

# create matrix using vectors as ROWS
matrix_row <- rbind(x0, y0)
matrix_row

# create matrix using vectors as COLUMNS
matrix_col <- cbind(x0, y0)
matrix_col



# Data Frame --------------------------------------------------------------

# can read in '.csv' files from URL (or locally), as well as HTML tables online (although might require some labor)
df_iris <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE)

# check out first few rows of `df_iris`
head(df_iris)

# get values in 10th row
df_iris[10,]

# get values in 5th column
df_iris$V5 # or `df_iris[5]`



# Example w/ `lm()` and Plotting ------------------------------------------

### finding the coefficients of a linear equation that best fits a set of data points (least squares)

# load in `ggplot2` to demonstrate its plotting capabilities
library(ggplot2)

# generate sample data
x <- 1:10
y <- 3 * x + rnorm(10) # y = 3x (with noise)

# create data frame with the x and y values
df <- data.frame(x = x, y = y)

# use built-in function `lm()` to fit a linear model
# similar to numpy.linalg.lstsq() (or scipy function) in Python
model <- lm(y ~ x)
b <- coef(model)[1]
m <- coef(model)[2]
predicted_y <- m*x + b

# plot the data using `ggplot2`
# highly customizable!
ggplot(data = df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = predicted_y), linetype = "dashed", color = "red") +
  labs(title = "Linear Regression", x = "X", y = "Y")



# Example w/ `qr.solve()` -------------------------------------------------

# functionality incorporated in `sparsifyDynamics.R`

# create matrix `A` and vector `b`
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
b <- c(7, 8)

# solve the system of equations 'Ax = b' using QR decomposition
# QR decomp. is a common matrix factorization technique
  # expresses `A` as product of orthonormal matrix Q and upper triangular matrix R
  # helps solve system more efficiently
x <- qr.solve(A, b)

# output the solution
x
