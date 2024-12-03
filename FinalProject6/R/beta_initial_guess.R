#' @title Beta Initial Guess
#'
#' @description Determine Initial Beta Coefficients for optimization using the
#' least-squares formula.
#' @param x A \code{character vector} containing the names of all features
#' we will train the binary classification on.
#' @param y A \code{character} containing the name of the classified feature
#' (which we will fit the model on).
#' @param data A \code{data frame} containing the data we will train the binary
#' classification on.
#' @return A $p$ x 1 \code{vector} containing the initial beta coefficient values,
#' where p is the number of features given plus 1.
#' @author Emily Knight
#' @examples
#' beta_initial_guess(x = c("age", "workclass", "hours.per.week"), y = "sex", data = adult)
beta_initial_guess <- function(x, y, data){
  ## Format user data
  list1 <- format_input(x, y, data)
  x <- list1$x
  y <- list1$y
  xx <- t(x)%*%x

  # Check for singularity
  if (det(xx) == 0) stop("Matrix X'X is singular and cannot be inverted.")

  xx <- solve(xx)

  return(xx %*% t(x) %*% y)

}
