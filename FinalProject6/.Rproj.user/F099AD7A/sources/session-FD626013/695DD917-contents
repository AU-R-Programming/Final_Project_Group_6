#' @title Pre-Train Data Formatting
#'
#' @description Format the data correctly so that it can be trained,
#' including changing the format of factors.
#' @param x A \code{character vector} containing the names of all features
#' we will train the binary classification on.
#' @param y A \code{character} containing the name of the classified feature
#' (which we will fit the model on).
#' @param data A \code{data frame} containing the data we will train the binary
#' classification on.
#' @return A \code{list} containing the following attributes, where n is the number
#' of observations in the given data frame and k is the number of provided features
#' to predict on:
#' \describe{
#'      \item{x}{An n x k+1 matrix containing the predictors we will use to train}
#'      \item{y}{An n x 1 vector contatining the classification of each observation}
#' }
#' @author Emily Knight
#' @examples
#' format_input(x = c("age", "workclass", "house.per.week"),
#'       y = "sex", data = adult)
format_input <- function(x, y, data){
# check user input
## generate an appropriate error if the corresponding column names not found or if data is not a data frame
# Check if data is a data frame
if (!is.data.frame(data)) {stop("The input 'data' must be a data frame.")}
if(!y %in% colnames(data)){stop(paste("The y column of interest,", y, "was not found in the data frame of interest. Please double check the name of the column you are interested in and try again!"))}
if(!all(x %in% colnames(data))){stop(paste("One of more of the x columns you are interested in was not found in the data frame of interest."))}

## extract x and y columns from the data frame
y <- data[[y]]
x <- data[ , x, drop = FALSE]

## format
## ensure y is treated as numeric, since using numerical optimization for binary classification
## ensure x is formatted to numeric with character / factors being converted to dummy variables
y <- as.factor(y)
y <- as.numeric(y) - 1
x <- model.matrix(~ ., data = x)

## store current output in list
list1 <- list(x = x, y = y)

return(list1)

}
