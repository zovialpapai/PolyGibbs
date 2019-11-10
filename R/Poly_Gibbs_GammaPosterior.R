# Input beta_matrix, burn_in, break, k
# gamma_update: a nIter X (K+1) matrix of beta_updates staring with -Inf and ending with + Inf, sorted in each row
# k: a integer not greater than (p+1) indicating which beta is of interest#*
# burn_in: burn_in period , less than (nrow(beta_matrix) - 1)
# breaks: integer, no of breaks in histogram

# May include default choices
#' Plots Posterior Frequency Distribution
#'
#'\code{Multinom_PosteriorDistribution_plot_beta} Plots Posterior Frequency Distribution of Boundary Parameters estimated by Ordered Multinomial Regression via data augmentation and Gibbs sampling.
#' @param gamma_update a nIter X (K+1) matrix of beta_updates staring with -Inf and ending with + Inf, sorted in each row
#' @param k a integer not greater than (p+1) indicating which beta is of interest
#' @param burn_in burn_in period , less than (nrow(beta_matrix) - 1)
#' @param breaks integer, no of breaks in histogram
#'
#' @return \code{PosteriorDistribution_plot} A histrogram showing Posterior Frequency Distribution and Posterior Mean
#' @export
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
#' @examples gamma_update = Result$gamma_update
#' @examples Multinom_PosteriorDistribution_plot_gamma(gamma_update = gamma_update , k = 2, burn_in = 2500, breaks= 50)


Multinom_PosteriorDistribution_plot_gamma <- function(gamma_update, k, burn_in, breaks){
  # Checking conformibility of gamma_update
  if(sum(gamma_update[ ,1] == (-Inf)) != nrow(gamma_update)){
    stop("1st column of gamma_update should be all -Inf")
  }
  if(sum(gamma_update[ ,ncol(gamma_update)] == (Inf)) != nrow(gamma_update)){#*
    stop("last column of gamma_update should be all Inf")
  }
  if(sum(apply(gamma_update[ ,-c(1,ncol(gamma_update))], 1, function(t){is.unsorted(t) })) != 0){
    stop(" The rows of gamma_update must be sorted")
  }
  # Checking Compatibilty of burn_in
  if(burn_in >= (nrow(gamma_update) - 1 )){
    stop("burn_in: burn_in period has to be less than (nrow(gamma_update) - 1)")
  }
  # checking validity of k
  #if( k %in% (0:(ncol(gamma_update) - 1)) == FALSE){#*
  if( k %in% (2:((ncol(gamma_update) - 1 ))) == FALSE){
    stop(" plot can be generated if k is in 2:((ncol(gamma_update) - 1 ))) ")
  }
  # Warning on no of breaks
  if( breaks > (nrow(gamma_update) - burn_in)){
    warning(" no. of breaks too large")
  }
  #calculating posterior means
  posterior_means = colMeans(gamma_update[-(1:burn_in),])
  # Drawing histogram and plotting posterior mean
  # hist(gamma_update[-(1:burn_in), (k + 1)], breaks = breaks, main = paste("Posterior Distribution of gamma_", k), xlab = paste("Values of gamma_", k), col = "green")
  # abline(v = posterior_means[(k + 1)], col = "red", lwd = 3)
  hist(gamma_update[-(1:burn_in), (k )], breaks = breaks, main = paste("Posterior Distribution of gamma_", k), xlab = paste("Values of gamma_", k), col = "green")
  abline(v = posterior_means[(k )], col = "red", lwd = 3)#*
  legend("topright", "Posterior Mean", lty = 1, lwd = 2, col = "red")
}

