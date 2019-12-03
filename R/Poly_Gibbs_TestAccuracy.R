# Input Predicted_Y, Test_Y (ordering is assumed)
# K: An integer giving no. of classes
#' Calculation of Accuracy of Prediction on Test Set for Bayesian Ordered Multinomial Regression
#'
#'\code{MultinomGibbs_Test_Accuracy} Calculates Accuracy of Prediction on Test Set for Bayesian Ordered Multinomial Regression.
#'
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param Predicted_Y A (length(Test_Y)) X 1 vector of prediction for testset responses.
#' @param Test_Y A (length(Test_Y)) X 1 vector of testset responses.
#' @param K An integer giving no. of classes.
#'
#' @return Test_Accuracy A real no between 0 and 100 giving accuracy of prediction in test set.
#' @examples # Initialization
#' @examples set.seed(250)
#' @examples n <- 1000 # Total no of observations.
#' @examples int1 <- -1 # gamma boundary
#' @examples int2 <- 3  # gamma boundary
#' @examples beta <- c(-.75, 1) # Regression Parameters for data generation.
#' @examples X <- cbind(sample(1:4, n, replace = TRUE), rnorm(n, 0, 2)) # Generated design matrix
#' @examples # Generation of Latent Variable Observations
#' @examples eta <- X %*% beta
#' @examples z <- rnorm(n, eta, 1)
#' @examples # Generation of Responses depending on z
#' @examples y <- rep(0, n)
#' @examples y[z <= int1] <- 1
#' @examples y[int1 <z & z <= int2] <- 2
#' @examples y[int2 < z ] <- 3
#' @examples #Spliting The Data in Train and Test in 80:20 ratio
#' @examples Train_ID = sample(1:nrow(X), round(nrow(X) * 0.8), replace = FALSE) # Train Data IDS
#' @examples Train_X = X[Train_ID, ]# Train Data Covariates
#' @examples Test_X = X[-Train_ID, ]
#' @examples Train_Y = y[Train_ID] # Train Data Response
#' @examples Test_Y = y[-Train_ID] # Test Data Response
#' @examples K = 3
#' @examples nIter = 10000
#' @examples burn_in = 5000
#' @examples Result = MultinomGibbs_fit(Train_X, Train_Y, nIter, burn_in, K)
#' @examples estimates = Result$estimates
#' @examples gamma_estimates = Result$gamma_estimates
#' @examples Result_Pred = MultinomGibbs_pred(estimates, gamma_estimates,Test_X )
#' @examples Predicted_Y = Result_Pred
#' @examples MultinomGibbs_Test_Accuracy(Predicted_Y, Test_Y, K)
#'
#' @export
#'

MultinomGibbs_Test_Accuracy <- function(Predicted_Y, Test_Y, K){
  # Check if Predicted_Y and  Test_Y are compatible
  if(length(Predicted_Y) != length(Test_Y)){
    stop(" Predicted_Y and  Test_Y are incompatible")
  }
  if(sum(Test_Y %in% c(0:K)) != length(Test_Y)){
    stop("Indexing in Test_Y must be in 1,2, ....K")
  }
  # Return output
  # Test_Accuracy: The percentage of prediction accuracy in test set
  Test_Accuracy = (sum(Predicted_Y == Test_Y)/ length(Test_Y)) * 100
  return(Test_Accuracy)
}

