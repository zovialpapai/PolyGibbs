# Input beta_matrix, k
# beta_matrix: a nIter X (p+1) matrix of beta_updates
# k: a integer not greater than (p+1) indicating which beta is of interest
#Libraries required
#install.packages("MASS")
#install.packages("truncnorm")
require(MASS)
require(truncnorm)


#' Plots Posterior Frequency Distribution .
#'
#' \code{BinaryGibbs_PosteriorDistribution_plot} Plots Posterior Frequency Distribution of Parameters estimated by Probit Regression for Binary Responses via data augmentation and Gibbs sampling.
#' @import MASS
#' @import truncnorm
#' @import graphics
#'
#' @param beta_matrix: a nIter X (p+1) matrix of beta_updates.
#' @param k: a integer not greater than (p+1) indicating which beta is of interest.
#'
#' @return \code{traceplot} Line diagrams showing convergence of gibbs sampler for a parameter and indicating cumulative posterior mean over iterartions.
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
#' @examples temp = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
#' @examples BinaryGibbs_Traceplot(beta_matrix = temp$beta_matrix, k = 0)
#'
#' @export

BinaryGibbs_Traceplot <- function(beta_matrix, k){
  # checking validity of k
  if( k %in% (0:(ncol(beta_matrix) - 1)) == FALSE){
    stop(" k is out of bound")
  }
  # Drawing the Traceplot
  plot(beta_matrix[ ,(k + 1)], type = "l", xlab = paste(" values of beta_", k) , main = paste("Traceplot of beta_", k))
  # Adding the cumulating mean
  lines(cumsum(beta_matrix[ , (k + 1)])/(1:nrow(beta_matrix)), col = "red", lwd = 2)
}
