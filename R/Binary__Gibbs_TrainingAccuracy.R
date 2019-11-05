# Test Accuracy -----------------------------------------------------------
# Input Predicted_Y, Test_Y (ordering is assumed)

#' Calculation of Accuracy of Prediction on Test Set for Bayesian Probit Regression.
#'
#' \code{BinaryGibbs_Test_Accuracy} Calculates Accuracy of Prediction on Test Set for Bayesian Probit Regression.
#' @param Predicted_Y: a nrow(Test_X) X 1 vector of predicted responses.
#' @param Test_Y: a nrow(Test_X) X 1 vector of actual responses.(ordering is assumed)
#'
#' @return Test_Accuracy: A scalar giving the accuracy on the test set.
#'
#' @examples set.seed(250)
#' @examples require(truncnorm)
#' @examples require(MASS)
#' @examples N <- 500
#' @examples x1 <- seq(-1, 1, length.out = N)
#' @examples x2 <- rnorm(N, 0, 1)
#' @examples D <- 3
#' @examples X <- matrix(c(rep(1, N), x1, x2), ncol = D)
#' @examples true_theta <- c(- 0.5, 3.3, 2)
#' @examples p <- pnorm(X %*% true_theta)
#' @examples y <- rbinom(N, 1, p)
#' @examples N1  <- sum(y)  # Number of successes
#' @examples N0  <- N - N1  # Number of failures
#' @examples #Spliting The Data in Train and Test in 80:20 ratio
#' @examples Train_ID = sample(1:nrow(X), round(nrow(X) * 0.8), replace = FALSE) # Train Data IDS
#' @examples Train_X = X[Train_ID, -1] # Train Data Covariates
#' @examples Test_X = X[-Train_ID, -1] # Test Data Covarites
#' @examples Train_Y = y[Train_ID] # Train Data Response
#' @examples Test_Y = y[-Train_ID] # Test Data Response
#' @examples nIter = 10000
#' @examples burn_in = round(nIter * 0.5)
#' @examples prior = 2
#' @examples prior_mean = rep(1, 3)
#' @examples prior_var = diag(10, 3)
#' @examples Output = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
#' @examples estimates = Output$estimates
#' @examples Predicted_Y = BinaryGibbs_Pred(estimates, Test_X)
#' @examples BinaryGibbs_Test_Accuracy(Predicted_Y, Test_Y)
#'
#' @export
BinaryGibbs_Test_Accuracy <- function(Predicted_Y, Test_Y){
  warning("Test_Y: a nrow(Test_X) X 1 vector of actual responses.(ordering is assumed)")
  # Check if Predicted_Y and  Test_Y are compatible
  if(length(Predicted_Y) != length(Test_Y)){
    stop(" Predicted_Y and  Test_Y are incompatible")
  }
  # Checking if classes in TestY are represented as 0 or 1 .
  sum(Test_Y %in% c(0,1) != length(Test_Y))
  if(sum(Test_Y %in% c(0,1)) != length(Test_Y)){
    stop("Indexing in Test_Y must be 0 and 1")
  }
  # Return output
  # Test_Accuracy: The percentage of prediction accuracy in test set
  Test_Accuracy = (sum(Predicted_Y == Test_Y)/ length(Test_Y)) * 100
  return(Test_Accuracy)
}
