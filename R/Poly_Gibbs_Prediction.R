# Prediction --------------------------------------------------------------
# Input estimates, gamma_estimates, Test_X
# estimates: a (p) X 1 vector of beta estimates, where beta1, beta2 etc are sorted as 0,1,2...
# gamma_estimates: a (K+1) X 1 vector of gamma estimates which starts and ends with -Inf and Inf respectively.
# Test_X: a () X p matrix of continuous scale covarites

#' Prediction using fitted Bayesian Ordered Multinomial Model
#' \code{MultinomGibbs_Pred} Predicts using fitted Bayesian Ordered Multinomial Model.
#'
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param estimates a (p) X 1 vector of beta estimates, where beta1, beta2 etc are sorted as 0,1,2...
#' @param gamma_estimates a (K+1) X 1 vector of gamma estimates which starts and ends with -Inf and Inf respectively and sorted in accending order.
#' @param Test_X a () X p matrix of continuous scale covarites
#'
#' @return Predicted_Y a nrow(Test_X) X 1 vector of predicted responses
#'
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
#' @examples MultinomGibbs_pred(estimates, gamma_estimates,Test_X )
#'
#' @export

MultinomGibbs_pred <- function(estimates, gamma_estimates,Test_X ){
# Defining parameters
K = length(gamma_estimates) - 1
# check if estimates and Test_X is incompatible
if(length(estimates) != (ncol(Test_X))){
  stop("estimates and Test_X is incompatible")
}
# Checking conformibility of gamma_update
if(gamma_estimates[1] != (-Inf)){
  stop("1st element of gamma_estimates should be -Inf")
}
if(gamma_estimates[length(gamma_estimates)] != (Inf)){
  stop("last element of gamma_estimates should be Inf")
}
if(is.unsorted(gamma_estimates)){
  stop(" gamma_estimates must be sorted in increasisng order")
}
# Caculation of Xbeta
X_beta = (as.matrix(Test_X)) %*% as.matrix(estimates)
# Storage for Predicted_Y
Predicted_Y = vector(length = nrow(Test_X))
# Prediction of Y
for(i in 1: nrow(Test_X)){
  for(j in 1:K){
    if(X_beta[i] >= gamma_estimates[j] && X_beta[i] <= gamma_estimates[j+1]){
      Predicted_Y[i] = j # Fitted value of Y_i
    }
  }
}
return(Predicted_Y)
}

