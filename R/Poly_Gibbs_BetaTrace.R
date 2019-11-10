#' Plots for Diagnosis of Convergence Distribution .
#'
#' \code{Multinom_traceplot_beta} Plots for diagnosis of Parameters estimates by Ordered Multinomial Regression via data augmentation and Gibbs sampling.
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param beta_matrix A nIter X p matrix of beta updates over all iterations.
#' @param k a integer not greater than (p) indicating which beta is of interest.
#'
#' @return \code{traceplot} Line diagrams showing convergence of gibbs sampler for a parameter and indicating cumulative posterior mean over iterartions.
#'
#' @export
#'
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
#' @examples k = 1
#' @examples nIter = 10000
#' @examples burn_in = 5000
#' @examples breaks = 50
#' @examples Result = MultinomGibbs_fit(Train_X, Train_Y, nIter, burn_in, K)
#' @examples beta_matrix = Result$beta_matrix
#' @examples Multinom_traceplot_beta(beta_matrix = beta_matrix, k = 1)

Multinom_traceplot_beta <- function(beta_matrix, k){
  # checking validity of k
  if( k %in% (1:(ncol(beta_matrix))) == FALSE){#*
    stop(" k is out of bound")
  }
  # Drawing the Traceplot#*
  plot(beta_matrix[ ,(k)], type = "l", xlab = paste(" values of beta_", k) , main = paste("Traceplot of beta_", k))
  # Adding the cumulating mean
  lines(cumsum(beta_matrix[ , (k)])/(1:nrow(beta_matrix)), col = "red", lwd = 2)
}

