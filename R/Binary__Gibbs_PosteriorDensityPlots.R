# Posterior Density Plots ----------------------------------
# Input beta_matrix, burn_in, break, k
# beta_matrix: a nIter X (p+1) matrix of beta_updates
# k: a integer not greater than (p+1) indicating which beta is of interest
# burn_in: burn_in period , less than (nrow(beta_matrix) - 1)
# breaks: integer, no of breaks in histogram

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
#' @param burn_in: burn_in period , less than (nrow(beta_matrix) - 1)
#' @param breaks: integer, no of breaks in histogram
#'
#' @return \code{PosteriorDistribution_plot} A histrogram showing Posterior Frequency Distribution and Posterior Mean
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
#' @examples BinaryGibbs_PosteriorDistribution_plot(beta_matrix = temp$beta_matrix , k = 0, burn_in = 2500, breaks= 50)
#'
#' @export

BinaryGibbs_PosteriorDistribution_plot <- function(beta_matrix, k, burn_in, breaks){
  # Checking Compatibilty of burn_in
  if(burn_in >= (nrow(beta_matrix) - 1 )){
    stop("burn_in: burn_in period has to be less than (nrow(beta_matrix) - 1)")
  }
  # checking validity of k
  if( k %in% (0:(ncol(beta_matrix) - 1)) == FALSE){
    stop(" k is out of bound")
  }
  # Warning on no of breaks
  if( breaks > (nrow(beta_matrix) - burn_in)){
    warning(" no. of breaks too large")
  }
  #calculating posterior means
  posterior_means = colMeans(beta_matrix[-(1:burn_in),])
  # Drawing histogram and plotting posterior mean
  hist(beta_matrix[-(1:burn_in), (k + 1)], breaks = breaks, main = paste("Posterior Distribution of beta_", k), xlab = paste("Values of beta_", k), col = "green")
  abline(v = posterior_means[(k + 1)], col = "red", lwd = 3)
  legend("topright", "Posterior Mean", lty = 1, lwd = 2, col = "red")
}

