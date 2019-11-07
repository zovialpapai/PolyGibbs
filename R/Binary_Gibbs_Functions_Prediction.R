# Prediction--------------------------------------------
# Input estimates, Test_X
# estimates: a (p+1) X 1 vector of beta estimates, where beta0, beta1, beta2 etc are sorted as 0,1,2...
# Test_X: a () X p matrix of continuous scale covarites

#' Prediction using fitted Bayesian Probit Model.
#'
#' \code{BinaryGibbs_Pred} Predicts using fitted Bayesian Probit Model.
#' @import MASS
#' @import truncnorm
#' @import graphics
#'
#' @param estimates: a (p+1) X 1 vector of beta estimates, where beta0, beta1, beta2 etc are sorted as 0,1,2...
#' @param Test_X: a () X p matrix of continuous scale covarites
#'
#' @return Predicted_Y: a nrow(Test_X) X 1 vector of predicted responses
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
#' @examples BinaryGibbs_Pred(estimates, Test_X)
#'
#' @export

BinaryGibbs_Pred <- function(estimates, Test_X){
  warning("estimates: a (p+1) X 1 vector of beta estimates, where beta0, beta1, beta2 etc are sorted as 0,1,2...")
  # check if estimates and Test_X is incompatible
  if(length(estimates) != (ncol(Test_X) + 1)){
    stop("estimates and Test_X is incompatible")
  }
  # Including intercept in test set
  Test_X = cbind(rep(1,nrow(Test_X)), Test_X)
  # Calculation of Xbeta in test set
  X_beta = (as.matrix(Test_X)) %*% as.matrix(estimates)
  # Storage for Predicted_Y
  Predicted_Y = vector(length = nrow(Test_X))
  for(i in 1:nrow(Test_X)){
    Predicted_Y[i] = rbinom(n = 1, size = 1, prob = pnorm(X_beta[i], mean = 0, sd = 1)) # Precdiction for ith obs
  }
  # Return output
  # Predicted_Y: a nrow(Test_X) X 1 vector of predicted responses
  return(Predicted_Y)
}
