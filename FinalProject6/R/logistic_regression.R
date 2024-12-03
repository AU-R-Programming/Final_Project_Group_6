#' @title Logistic Regression
#'
#' @description Given input data, run logistic regression such that the loss
#' function specified in the instructions is minimized and return data trained on,
#' list of feature names, and initial and optimal beta vectors.
#' @param x A \code{character vector} containing the names of all features
#' we will train the binary classification on.
#' @param y A \code{character} containing the name of the classified feature
#' (which we will fit the model on).
#' @param data A \code{data frame} containing the data we will train the binary
#' classification on.
#' @return A \code{list} containing these two lists, where n is the number
#' of observations in the given data frame and k is the number of provided features
#' to predict on:
#' \describe{
#' \item{user_data}{A \code{list} containing the following attributes:
#' to predict on:
#'      \describe{
#'           \item{x}{An n x k+1 \code{matrix} containing the predictors we used to train}
#'           \item{y}{An n x 1 \code{vector} containing the classification of each observation}
#'         }
#'    }
#' \item{coefficients}{A \code{list} containing the following attributes:
#'      \describe{
#'           \item{Coefficient}{A \code{character vector} of length k+1 containing the names
#'           of all features logistic regression was trained on (including intercept)}
#'           \item{beta_initial}{A k+1 x 1 \code{vector} which is the initial coefficient
#'           values for optimization using the least squares formula}
#'           \item{beta_estimate}{A k+1 x 1 \code{vector} which is the optimal beta hat
#'           coefficient vector calculated by optimizing the provided loss function}
#'      }
#'    }
#' }
#' @author Emily Knight
#' @export
#' @examples
#' logistic_regression(x = c("age", "workclass", "house.per.week"),
#'          y = "sex", data = adult)
logistic_regression <- function(x, y, data){

  list1 <- format_input(x = x, y = y, data = data)

  # Determine Initial Beta Coefficients
  beta_initial <- beta_initial_guess(x, y, data)

  # set initial epsilon_applied to FALSE for use in generating a warning if used in the loss function
  epsilon_applied <<- FALSE

  # estimate coefficients
  optimal_result <- optim(par = beta_initial,
                          fn = loss_function,
                          x = list1$x,
                          y = list1$y,
                          method = "BFGS")

  beta_estimate <- optimal_result$par

  # update list
  list2 <- list(Coefficient = colnames(list1$x), beta_initial = beta_initial,  beta_estimate = beta_estimate)
  my_output <- list(user_data = list1, coefficients = list2)

  # provide user with relevant output
  print(my_output$coefficients)
  return(my_output)
}
