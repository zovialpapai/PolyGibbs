# Input beta_matrix, burn_in, break, k
# beta_matrix: a nIter X (p) matrix of beta_updates
# k: a integer not greater than (p) indicating which beta is of interest#*
# burn_in: burn_in period , less than (nrow(beta_matrix) - 1)
# breaks: integer, no of breaks in histogram

# May include default choices
#' Plots Posterior Frequency Distribution
#'
#' \code{Multinom_PosteriorDistribution_plot_beta} Plots Posterior Frequency Distribution of Parameters estimated by Ordered Multinomial Regression via data augmentation and Gibbs sampling.
#'
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param beta_matrix a nIter X (p) matrix of beta_updates.
#' @param k a integer not greater than (p) indicating which beta is of interest.
#' @param burn_in burn_in period , less than (nrow(beta_matrix) - 1)
#' @param breaks integer, no of breaks in histogram
#'
#' @return \code{PosteriorDistribution_plot} A histrogram showing Posterior Frequency Distribution and Posterior Mean
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
#' @examples breaks = 50
#' @examples Result = MultinomGibbs_fit(Train_X, Train_Y, nIter, burn_in, K)
#' @examples beta_matrix = Result$beta_matrix
#' @examples Multinom_PosteriorDistribution_plot_beta(beta_matrix = beta_matrix , k = 2, burn_in = 2500, breaks= 50)
#'
#'@export

Multinom_PosteriorDistribution_plot_beta <- function(beta_matrix, k, burn_in, breaks){
  # Checking Compatibilty of burn_in
  if(burn_in >= (nrow(beta_matrix) - 1 )){
    stop("burn_in: burn_in period has to be less than (nrow(beta_matrix) - 1)")
  }
  # checking validity of k
  #if( k %in% (0:(ncol(beta_matrix) - 1)) == FALSE){#*
  if( k %in% (1:(ncol(beta_matrix) )) == FALSE){
    stop(" k is out of bound")
  }
  # Warning on no of breaks
  if( breaks > (nrow(beta_matrix) - burn_in)){
    warning(" no. of breaks too large")
  }
  #calculating posterior means
  posterior_means = colMeans(beta_matrix[-(1:burn_in),])
  # Drawing histogram and plotting posterior mean
  hist(beta_matrix[-(1:burn_in), (k )], breaks = breaks, main = paste("Posterior Distribution of beta_", k), xlab = paste("Values of beta_", k), col = "green")
  abline(v = posterior_means[(k )], col = "red", lwd = 3)#*
  legend("topright", "Posterior Mean", lty = 1, lwd = 2, col = "red")
}
