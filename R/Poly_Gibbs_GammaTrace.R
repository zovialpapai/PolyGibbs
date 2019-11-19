#' Plots for Diagnosis of Convergence Distribution .
#'
#' \code{Multinom_traceplot_gamma} Plots for diagnosis of Parameters estimates by Ordered Multinomial Regression via data augmentation and Gibbs sampling.
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param gamma_update a nIter X (K+1) matrix of beta_updates staring with -Inf and ending with + Inf, sorted in each row
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
#' @examples gamma_update = Result$gamma_update
#' @examples Multinom_traceplot_gamma(gamma_update = gamma_update , k = 2)

Multinom_traceplot_gamma <- function(gamma_update, k){
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
  # checking validity of k
  #if( k %in% (0:(ncol(gamma_update) - 1)) == FALSE){#*
  if( k %in% (2:((ncol(gamma_update) - 1 ))) == FALSE){
    stop(" plot can be generated if k is in 2:((ncol(gamma_update) - 1 ))) ")
  }
  # Warning on no of breaks
  if( breaks > (nrow(gamma_update) - burn_in)){
    warning(" no. of breaks too large")
  }
  # Drawing the Traceplot#*
  plot(gamma_update[ ,(k)], type = "l", xlab = paste(" values of gamma_", k) , main = paste("Traceplot of gamma_", k))
  # Adding the cumulating mean
  lines(cumsum(gamma_update[ , (k)])/(1:nrow(gamma_update)), col = "red", lwd = 2)
}

