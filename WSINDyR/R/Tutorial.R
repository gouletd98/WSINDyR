### Tutorial.R (APPM 4720/5720)
### Adam Manaster and Derek Goulet
### Last Edit on 04/25/2023

# Basic Function ----------------------------------------------------------

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



# Data Frame --------------------------------------------------------------

# can read in '.csv' files from URL (or locally), as well as HTML tables online (although might require some labor)
df_iris <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header = FALSE)

# check out first few rows of `df_iris`
head(df_iris)

# get values in 10th row
df_iris[10,]

# get values in 5th column
df_iris$V5 # or ... `df_iris[5]`



# Example w/ `lstsq()` and Plotting ---------------------------------------

### finds the coefficients of a linear equation that best fits a set of data points

# load in `ggplot2` to demonstrate its plotting capabilities
library(ggplot2)
library(prama)

# Define the lstsq() function
lstsq <- function(X, Y) {
  solve(t(X) %*% X) %*% t(X) %*% Y
}

# generate sample data
x <- 1:10
y <- 3 * x + rnorm(10)

# create data frame with the x and y values
df <- data.frame(x = x, y = y)

# use `lstsq()`` to fit a linear model
X <- cbind(1, x)
b <- lstsq(x, y)
predicted_y <- lm(y ~ x)
predicted_y <- x %*% b

# plot the data using ggplot2
ggplot(data = df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_line(aes(y = predicted_y), linetype = "dashed") +
  labs(title = "Linear Regression", x = "x", y = "y")



# Example w/ `qr.solve()` -------------------------------------------------

# sparsifyDynamics.R

