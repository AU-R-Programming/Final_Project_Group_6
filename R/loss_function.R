#' @title Loss Function
#'
#' @description Define the loss function that we will use to optimize for
#' binary classification.
#' @param beta A \code{vector} containing the beta that we will evaluate the
#' loss function for.
#' @param x A \code{matrix} containing the formatted feature data.
#' @param y A \code{vector} containing the classifications of the features.
#' @return A \code{numeric} which is the log likelihood value.
#' @author Emily Knight
loss_function <- function(beta, x, y){
  p <- 1 / (1 + exp(-x %*% beta))

  # Check if any p is exactly 0 or 1, which would cause log(0) or log(1) errors

  if (any(p == 0 | p == 1)) {
    epsilon <- 1e-15
    p <- pmax(pmin(p, 1 - epsilon), epsilon)

    # Notify the user if epsilon was applied
    if (!epsilon_applied) {
      warning("Log(0) or Log(1) encountered. Epsilon was applied to probabilities to avoid numerical errors.")
      epsilon_applied <<- TRUE
    }
  }

  # Negative log-likelihood
  log_likelihood <- -sum(y * log(p) + (1 - y) * log(1 - p))

  return(log_likelihood)
}

