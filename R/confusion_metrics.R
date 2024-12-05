#' @title Confusion Metrics
#'
#' @description Given coefficient vector beta, feature data, and classifications, output the
#' predictions of logistic regression with the correct classifications in a confusion matrix.
#' Also output prevalence, accuracy, sensitivity, specificity, false discovery rate, and
#' diagnostic odds ratio.
#' @param beta A \code{vector} of size p x 1, where p is the number of features in the data
#' (including the intercept), representing the coefficients we will use to predict with logistic
#' regression.
#' @param x A \code{matrix} of size n x p, where n is the number of observations of the data
#' and $p$ is the number of features (including the intercept).
#' @param y A \code{vector} of size n x 1 representing the classification of each of the n
#' observations, 0 or 1.
#' @return A \code{list} containing the following attributes:
#' \describe{
#' \item{ConfusionMatrix}{A \code{matrix} of integers that represent how many of each classification
#' logistic regression classified correctly and incorrectly using the provided beta vector}
#' \item{Metrics}{A \code{list} of six numeric values which are, in order:
#'      \describe{
#'           \item{Prevalence}{}
#'           \item{Accuracy}{}
#'           \item{Sensitivity}{}
#'           \item{Specificity}{}
#'           \item{FalseDiscoveryRate}{}
#'           \item{DiagnosticOddsRatio}{}
#'      }
#'      }
#'    }
#' @author Shuqi Du
#' @export
#' @examples
#' my_result <- logistic_regression(x = c("age", "workclass", "hours.per.week")
#'     , y = "sex", data = adult)
#' confusion_metrics(beta = my_result$coefficients$beta_estimate,
#'  x = my_result$user_data$x, y = my_result$user_data$y)
#'
#' (see logistic_regression help file for help with formatting)
confusion_metrics <- function(beta, x, y) {
  # Calculate predicted probabilities
  p <- 1 / (1 + exp(-x %*% beta))

  # Convert probabilities to binary predictions using a cutoff of 0.5
  y_pred <- ifelse(p > 0.5, 1, 0)

  # Create confusion matrix
  tp <- sum(y_pred == 1 & y == 1)
  tn <- sum(y_pred == 0 & y == 0)
  fp <- sum(y_pred == 1 & y == 0)
  fn <- sum(y_pred == 0 & y == 1)

  confusion_matrix <- matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE)
  rownames(confusion_matrix) <- c("Predicted 1", "Predicted 0")
  colnames(confusion_matrix) <- c("Actual 1", "Actual 0")

  # Calculate metrics
  prevalence <- (tp + fn) / (tp + tn + fp + fn)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  false_discovery_rate <- fp / (tp + fp)
  diagnostic_odds_ratio <- (tp / fn) / (fp / tn)

  metrics <- list(
    Prevalence = prevalence,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    FalseDiscoveryRate = false_discovery_rate,
    DiagnosticOddsRatio = diagnostic_odds_ratio
  )

  return(list(ConfusionMatrix = confusion_matrix, Metrics = metrics))
}

