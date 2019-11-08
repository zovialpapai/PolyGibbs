# Spliting The Data in Train and Test in some ratio
# Input :
# Trainset_Percentage (A real no between 0 and 1 both exclusive)
# X : A N X p matrix of covariates
# y : A N X 1 vector of responses


#' Spliting Data in Trainset and Test Set.
#'
#' \code{Split} Splits Data in Trainset and Test Set
#' @import MASS
#' @import truncnorm
#' @import mvtnorm
#' @import graphics
#' @import stats
#'
#' @param X  A N X p matrix of covariates
#' @param y  A N X 1 vector of responses
#' @param Trainset_Proportion (A real no between 0 and 1 both exclusive)
#'
#' @return \code{Train_X} A [N * Trainset_Proportion] X p matrix of covariates.
#' @return \code{Train_Y} A [N * Trainset_Proportion] X 1 vector of responses.
#' @return \code{Test_X} A [N * (1 - Trainset_Proportion)] X p matrix of covariates.
#' @return \code{Test_Y} A [N * (1 - Trainset_Proportion)] X 1 vector of responses.
#'
#' @examples set.seed(250)
#' @examples N <- 500 # Total no of observations.
#' @examples x1 <- seq(-1, 1, length.out = N) # Generating Covariate 1
#' @examples x2 <- rnorm(N, 0, 1) # Genearting Covariate 2
#' @examples # Generating the design matrix of interest
#' @examples X <- as.matrix(cbind(rep(1,N), x1, x2))
#' @examples # Fixing values of regression coeffiecients theta
#' @examples theta <- c(-.5, 3.3, 2)
#' @examples # Define a vector with probabilities of success p using the probit link
#' @examples p <- pnorm(X %*% theta)
#' @examples # Generate binary response data y with success probabilit p
#' @examples y <- rbinom(N, 1, p)
#' @examples split(X, y, Trainset_Proportion = .7)
#'
#' @export
#Spliting The Data in Train and Test in some ratio
# Input :
# Trainset_Percentage: (A real no between 0 and 1 both exclusive)
# X : A n X p matrix of covariates
# y : A n X 1 vector of responses
split <- function(X, y, Trainset_Proportion){
  #Checking if Trainset_Proportion is acceptable
  if((Trainset_Proportion > 0) * (Trainset_Proportion < 1) != 1 ){
    stop("Trainset_Percentage (A real no between 0 and 1 both exclusive)")
  }
  if(floor(Trainset_Proportion * nrow(X)) != Trainset_Proportion * nrow(X)){
    warning("Trainset_Proportion * nrow(X) is not integer , so it is floored")
  }
  #Checking if X and y are compatible
  if(nrow(X) != length(y)){
    stop(" X and y are incompatible")
  }
  Train_ID = sample(1:nrow(X), round(nrow(X) * Trainset_Proportion), replace = FALSE) # Train Data IDS
  Train_X = X[Train_ID, ] # Train Data Covariates
  Test_X = X[-Train_ID, ] # Test Data Covarites
  Train_Y = y[Train_ID] # Train Data Response
  Test_Y = y[-Train_ID] # Test Data Response
  return(list(Train_X = Train_X,Train_Y = Train_Y, Test_X = Test_X, Test_Y = Test_Y))
  # Train_X : A [N * Trainset_Proportion] X p matrix of covariates.
  # Train_Y : A [N * Trainset_Proportion] X 1 vector of responses.
  # Test_X : A [N * (1 - Trainset_Proportion)] X p matrix of covariates.
  # Test_Y :  A [N * (1 - Trainset_Proportion)] X 1 vector of responses.
}
