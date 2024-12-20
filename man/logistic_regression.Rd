% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_regression.R
\name{logistic_regression}
\alias{logistic_regression}
\title{Logistic Regression}
\usage{
logistic_regression(x, y, data)
}
\arguments{
\item{x}{A \code{character vector} containing the names of all features
we will train the binary classification on.}

\item{y}{A \code{character} containing the name of the classified feature
(which we will fit the model on).}

\item{data}{A \code{data frame} containing the data we will train the binary
classification on.}
}
\value{
A \code{list} containing these two lists, where n is the number
of observations in the given data frame and k is the number of provided features
to predict on:
\describe{
\item{user_data}{A \code{list} containing the following attributes:
to predict on:
     \describe{
          \item{x}{An n x k+1 \code{matrix} containing the predictors we used to train}
          \item{y}{An n x 1 \code{vector} containing the classification of each observation}
        }
   }
\item{coefficients}{A \code{list} containing the following attributes:
     \describe{
          \item{Coefficient}{A \code{character vector} of length k+1 containing the names
          of all features logistic regression was trained on (including intercept)}
          \item{beta_initial}{A k+1 x 1 \code{vector} which is the initial coefficient
          values for optimization using the least squares formula}
          \item{beta_estimate}{A k+1 x 1 \code{vector} which is the optimal beta hat
          coefficient vector calculated by optimizing the provided loss function}
     }
   }
}
}
\description{
Given input data, run logistic regression such that the loss
function specified in the instructions is minimized and return data trained on,
list of feature names, and initial and optimal beta vectors.
}
\examples{
logistic_regression(x = c("age", "workclass", "house.per.week"),
         y = "sex", data = adult)
}
\author{
Emily Knight
}
