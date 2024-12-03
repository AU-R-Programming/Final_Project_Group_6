#' @title Bootstrapping
#'
#' @description Given input data, confidence level, and number of runs, run the bootstrap
#' procedure to obtain confidence intervals for beta estimates.
#' @param x A \code{character vector} containing the names of all features
#' we will train the binary classification on.
#' @param y A \code{character} containing the name of the classified feature
#' (which we will fit the model on).
#' @param data A \code{data frame} containing the data we will train the binary
#' classification on.
#' @param alpha A \code{numeric} representing the significance level for the 1 - alpha
#' confidence intervals. Default set to 0.05.
#' @param B A \code{integer} representing the number of of bootstraps to run. Default
#' set to 20.
#' @return A \code{list} containing the following attributes, where $k$ is the number of
#' provided features to predict on and B is the number of bootstraps:
#' \describe{
#'      \item{Coefficients}{Confused by the setup of this matrix...
#'
#'      STILL NEED TO DO
#'      }
#'      \item{ConfidenceIntervals}{A $3$ x $k+1$ matrix containing the confidence intervals for
#'      beta for each feature. The first row, Lower, contains the lower estimate; the second row,
#'      Estimate, contains the mean from the bootstrap procedure; the third row, Upper, contains
#'      the upper estimate.}
#' }
#' @author Emily Knight
#' @examples
#' bootstrapping(x = c("age", "workclass", "hours.per.week"), y = "sex",
#'  data = adult, alpha = 0.1, B = 100)
bootstrapping <- function(x, y, data, alpha = 0.05, B = 20) {

  ## Format user data
  list1 <- format_input(x, y, data)
  x <- list1$x
  y <- list1$y

  # Determine Initial Beta Coefficients
  beta_initial_guess2 <- function(x, y){
    xx <- t(x)%*%x
    # Check for singularity
    if (det(xx) == 0) stop("Matrix X'X is singular and cannot be inverted.")
    xx <- solve(xx)
    return(xx %*% t(x) %*% y)
  }

  ## Create matrices to store the results
  beta_bootstrap_estimates <- matrix(NA, nrow = B, ncol = ncol(x)) # Store optimized coefficients
  beta_bootstrap_initial <- matrix(NA, nrow = B, ncol = ncol(x))  # Store initial guesses
  n <- nrow(data)

  ## Perform bootstrapping
  for (i in 1:B) {
    # Resample data
    index <- sample(1:n, size = n, replace = TRUE)
    x_boot <- x[index, , drop = FALSE]
    y_boot <- y[index]

    # Calculate initial beta for resampled data
    beta_boot <- beta_initial_guess2(x = x_boot, y = y_boot)
    beta_bootstrap_initial[i, ] <- beta_boot

    # Optimize to find "optimal" beta
    boot_optim <- optim(par = beta_boot,
                        fn = loss_function,
                        x = x_boot,
                        y = y_boot,
                        method = "BFGS")
    beta_bootstrap_estimates[i, ] <- boot_optim$par
  }

  ## Compute Confidence Intervals (Percentile Method)
  lower_bound <- apply(beta_bootstrap_estimates, 2, function(est) quantile(est, probs = alpha / 2))
  upper_bound <- apply(beta_bootstrap_estimates, 2, function(est) quantile(est, probs = 1 - alpha / 2))
  estimate <- colMeans(beta_bootstrap_estimates) # Average bootstrap estimates

  ## Combine Results into Lists
  coefficients_mat <- rbind(
    Beta_Bootstrap_Estimates = beta_bootstrap_estimates,
    Beta_Bootstrap_Initial = beta_bootstrap_initial
  )
  colnames(coefficients_mat) <- colnames(x)

  confidence_intervals_mat<- rbind(
    Lower = lower_bound,
    Estimate = estimate,
    Upper = upper_bound
  )
  colnames(confidence_intervals_mat) <- colnames(x)

  ## Output
  result <- list(
    Coefficients = coefficients_mat,
    ConfidenceIntervals = confidence_intervals_mat
  )

  return(result)
}
