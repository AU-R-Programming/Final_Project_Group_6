graph_ci <- function(x, y, data, alpha = 0.05, B = 20) {
  # Extract bootstrap estimates
  bootstrap_result <- bootstrapping(x = x, y = y, data = data, alpha = alpha, B = B)
  beta_bootstrap_estimates <- bootstrap_result$Coefficients[, , drop = FALSE]

  # Prepare data for boxplot
  coefficients <- colnames(beta_bootstrap_estimates)
  num_coefficients <- length(coefficients)

  # Set up the plot area
  boxplot(
    beta_bootstrap_estimates,
    names = coefficients,
    main = "Bootstrap Confidence Intervals for Coefficients",
    xlab = "Coefficient",
    ylab = "Beta Estimates",
    las = 2,  # Rotate x-axis labels for readability
    col = "lavender",
    border = "navy"
  )

  # Add grid for better readability
  grid(nx = NA, ny = NULL, col = "ghostwhite", lty = "dotted")
}

