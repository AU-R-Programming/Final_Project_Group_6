#' @title Graph Bootstrap Boxplots
#'
#' @description Run the bootstrapping procedure with the bootstrapping function
#' and produce boxplots.
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
#' @return Produces a graph with p boxplots, where p is the number of features you ran the bootstrap
#' procedure on plus the intercept. The y axis shows the value for the beta feature, and the
#' x axis has the p boxplots. Also returns the output of the bootstrapping procedure (see
#' help documentation for bootstrapping function for details.)
#' @author Emily Knight
#' @export
#' @examples
#' graph_ci(x = c("age", "workclass", "hours.per.week"),
#'      y = "sex", data = adult, alpha = 0.1, B = 100)
graph_ci <<- function(x, y, data, alpha = 0.05, B = 20) {

  # Extract bootstrap estimates
  bootstrap_result <- bootstrapping(x = x, y = y, data = data, alpha = alpha, B = B)
  beta_bootstrap_estimates <- bootstrap_result$Coefficients$Beta_Bootstrap_Estimates[, , drop = FALSE]

  # Prepare data for boxplot
  coefficients <- colnames(beta_bootstrap_estimates)
  num_coefficients <- length(coefficients)

  # Set up the plot area
  plot1<- (boxplot(
    beta_bootstrap_estimates,
    names = coefficients,
    main = "Bootstrap Coefficient Estimate Boxplots",
    xlab = "Coefficient",
    ylab = "Beta Estimates",
    las = 2,  # Rotate x-axis labels for readability
    col = "lavender",
    border = "navy"
  )

 +
  grid(nx = NA, ny = NULL, col = "ghostwhite", lty = "dotted"))

 return(plot1)
}
