
Logistic_Regression <- function(x, y, df){
  
  # check user input
  ## generate an appropriate error if the corresponding column names not found or if df is not a data frame
  # Check if df is a data frame
  if (!is.data.frame(df)) {stop("The input 'df' must be a data frame.")}
  if(!y %in% colnames(df)){stop(paste("The y column of interest,", y, "was not found in the data frame of interest. Please double check the name of the column you are interested in and try again!"))}
  if(!all(x %in% colnames(df))){stop(paste("One of more of the x columns you are interested in was not found in the data frame of interest."))}
  
  ## extract x and y columns from the data frame
  y <- df[[y]]
  x <- df[ , x, drop = FALSE]
  
  ## format
  ## ensure y is treated as numeric, since using numerical optimization for binary classification
  ## ensure x is formatted to numeric with character / factors being converted to dummy variables
  y <- as.factor(y)
  y <- as.numeric(y) - 1
  x <- model.matrix(~ ., data = x)
  
  ## store current output in list
  function_list <- list(x = x, y = y)
  
  # Determine Initial Beta Coefficients
  Beta_initial_guess <- function(x, y){
    
    xx <- t(x)%*%x
    
    # Check for singularity
    if (det(xx) == 0) stop("Matrix X'X is singular and cannot be inverted.")
    
    xx <- solve(xx)
    
    return(xx %*% t(x) %*% y)
    
    }
  
  beta_initial <- Beta_initial_guess(x = function_list$x, y = function_list$y)
  
  ## update list
  function_list$beta_initial <- beta_initial
  
  # set initial epsilon_applied to FALSE for use in generating a warning if used in the loss function
  epsilon_applied <- FALSE
  
  # Loss function: Negative log-likelihood
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


  
  
  ## estimate coefficients
  optimal_result <- optim(par = function_list$beta_initial,
                          fn = loss_function,
                          x = function_list$x,
                          y = function_list$y,
                          method = "BFGS")

  
  
  ## update list final format
    my_list1 <- list(x = x, y = y)
    beta_estimate <- optimal_result$par
    my_list2 <- list(beta_estimate = beta_estimate, beta_initial = beta_initial)
    my_output <- list(user_data = my_list1,
                     coefficients = my_list2)
  
  # provide user with relevant output  
    print(my_output$coefficients)
    return(my_output)
    
}


# example for testing output
adult <- read.csv("adult.csv", sep=";")
results1 <- (Logistic_Regression(x = c("age", "workclass", "hours.per.week"), y = "sex", df = adult))

