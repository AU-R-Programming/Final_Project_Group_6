
Configure <- function(x, y, df){
  
  # Configure X matrix and Y vector
  ## generate an appropriate error if the corresponding column names not found
  if(!y %in% colnames(df)){stop(paste("The y column of interest,", y, "was not found in the data frame of interest. Please double check the name of the column you are interested in and try again!"))}
  if(!all(x %in% colnames(df))){stop(paste("One of more of the x columns you are interested in was not found in the data frame of interest."))}
  
  ## subset
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
    xx <- solve(xx)
    return(xx %*% t(x) %*% y)
    }
  
  beta_initial <- Beta_initial_guess(x = function_list$x, y = function_list$y)
  
  ## update list
  function_list$beta_initial <- beta_initial
  
  # create loss function and estimate coefficients
  ## create loss function
  loss_function <- function(x, y, beta){
 
    p <- 1 / (1 + exp(- x %*% beta))
  
    beta_estimate <- sum((-y*log(p)) + (1 - y) * log(1 - p))
  
    }
  ## estimate coefficients
  optimal_result <- optim(par = function_list$beta_initial,
                          fn = loss_function,
                          y = function_list$y,
                          x = function_list$x)

  
  
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
results1 <- (Configure(x = c("age", "workclass", "hours.per.week"), y = "sex", df = adult))

