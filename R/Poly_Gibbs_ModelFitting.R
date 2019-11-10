# Model Fitting -----------------------------------------------------------
# Installing required packages
# install.packages("mvtnorm")
# install.packages("MASS")
# install.packages("truncnorm")
library(mvtnorm)
library(MASS)
library(truncnorm)
# Input: Train_X, Train_Y, nIter, burn_in
# Train_X: n X p matrix of continuous covarites of training set.
# Train_Y: n X 1 vector of responses of training set. (takes values 1, 2, 3, . K)
# nIter: An  integer, No of iterations for the Gibbs Sampler
# burn_in: An integer, No of iterations neglected at begining of the chain in calculation of posterior mean
# K: no of categories. (1,2,3,..K)
#'  Fitting Bayesian Ordered Multinomial Regression
#'
#' \code{MultinomGibbs_fit} does Implementation of Multinomial Regression for Ordered Categorical Responses via data augmentation and Gibbs sampling.
#' @import MASS
#' @import truncnorm
#' @import graphics
#' @import stats
#' @import mvtnorm
#'
#' @param Train_X  n X p matrix of continuous covarites of training set. (Without Intercept)
#' @param Train_Y  n X 1 vector of responses of training set. (takes values 1, 2, 3, . K)
#' @param nIter An  integer, No of iterations for the Gibbs Sampler
#' @param burn_in An integer, No of iterations neglected at begining of the chain in calculation of posterior mean
#' @param K no of categories. (1,2,3,..K)
#'
#' @return \code{estimates} A p X 1 vector of estimated posterior mean.
#' @return  \code{gamma_estimates} A (K + 1) X 1 vector of estimated gamma boundaries of Latent Variable Z, starting with -Inf and ending with +Inf.
#' @return \code{Train_Accuracy} A nIter X 1 vector of Training Accuracy over all iterations.
#' @return \code{beta_matrix} A nIter X p matrix of beta updates over all iterations.
#' @return \code{gamma_update} A nIter X (K + 1) matrix of gamma updates over all iterations with first and last columns being all -Inf s and +Inf s respectively.
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
#' @examples MultinomGibbs_fit(Train_X, Train_Y, nIter, burn_in, K)
#'
#' @export

MultinomGibbs_fit <- function(Train_X, Train_Y, nIter, burn_in, K){
# Statutory Warning
warning("Algorithm fits an without intercept model")
#Determining Data Dimensions
n = nrow(Train_X) # No of observations in trainging data covariates
p = ncol(Train_X) # No of Covariate Variables
#Doing Compatibility Checks on the Inputs
# Check if K >= 3
if(K <= 2){
  stop(" K has to be greater than 2")
}
# Checking if burn_in is less than nIter
if(nIter <= (burn_in)){
  stop("burn_in must be strictly less than (nIter)")
}
# Checking if n is larger than p
if(n <= p){
  stop("no. of observations should be strictly larger than no. of parameters")
}
# Checking if Train_X and Train_Y are compatible
if(nrow(Train_X) != length(Train_Y) ){
  stop("TrainX and TrainY are incompatible: Dimensions do not match")
}
# Checking if all K classes are represented in TrainY
if(length(unique(Train_Y)) != K ){
  stop("Must have observations from both classes in TrainY")
}
# Checking if classes in TrainY are represented as 1,2,3.. K
if(sum(Train_Y %in% c(1:K)) != length(Train_Y) ){
  stop("classes in TrainY must be represented as 1,2,3.. K")
}
## Including intercept into the model
#Train_X = cbind(rep(1, nrow(Train_X)), Train_X)#*
p = p - 1#*
#Declaring variables used in Gibbs Sampling
beta_matrix = matrix(NA, nrow = nIter, ncol = (p + 1) ) # Storage for beta updates over iterations
gamma_update = matrix(rep(c(-Inf,1:(K - 1), Inf),nIter), nrow = nIter, ncol = (K + 1), byrow = TRUE )
Z_Latent = vector(length = n) # Vector to store latent variables
Y_fitted = vector(length = n) # Vector to store fitted values of y
X_beta = vector(length = n) # Vector for storage, neccessary for calculating accuracy.
Train_Accuracy = vector(length = nIter) # Vector to store Percentage Accuracy

# Gibbs Sampler Upadation
# Checking if t(Train_X)%*%Train_X is computationally singular
kernel = (t(as.matrix(Train_X)) %*% as.matrix(Train_X)) # A required matrix of importance
if(abs(det(kernel)) < 0.001 ){
  stop("t(Train_X)%*%Train_X is computationally singular")
}
# Initializing regression parameter beta
kernel_inverse = solve(kernel) # Inverting kernel matrix
#beta_start = (kernel_inverse %*% (t(as.matrix(Train_X)) %*% as.vector(Train_Y))) # Calculating initial beta
beta_start = rep(0, (p+1))
beta_matrix[1, ] = beta_start # Storing initial beta
# Initializing boundary parameter gamma
gamma_start = c(-Inf,  1:(K-1), Inf)
gamma_update[1, ] = gamma_start
# gamma is assumed to be gamma_1, gamma_2,..,gamma_K+1
# Initial Generation of latent varible Z based on Train_Y
for(i in 1: length(Train_Y)){
  mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_start))) # Determing mean of the Latent varible Zi
  for(j in 1:K){
    if(Train_Y[i] == j){
      Z_Latent[i] = rtruncnorm(1, a = gamma_start[j], b = gamma_start[j + 1], mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
    }
  }
}

iteration = 2
for(iteration in 2:nIter){
  # Updating beta using full conditional of beta given Z, Train_Y
  mean_beta_iter_i = kernel_inverse %*% (t(as.matrix(Train_X)) %*% as.matrix(Z_Latent))
  beta_matrix[iteration, ] = mvrnorm(1, mean_beta_iter_i,  kernel_inverse)
  # Updation of gamma_update
  # gamma is assumed to be gamma_1, gamma_2,..,gamma_K+1
  for( j in 1:(K - 1)){
    LL = max(c(max(Z_Latent[which(Train_Y == j) ]), gamma_update[(iteration - 1), j])) # Lower limit of support of gamma_j+1
    UL = min(c(min(Z_Latent[which(Train_Y == (j + 1))]), gamma_update[(iteration - 1), j + 2])) # Upper limit of support of gamma_j+1
    gamma_update[iteration, j+1] = runif(1, LL, UL)# generating gamma_j+1
  }
  # Updation of latent variable Zis
  for(i in 1: length(Train_Y)){
    mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_matrix[iteration, ]))) # Determing mean of the Latent varible Zi
    for(j in 1:K){
      if(Train_Y[i] == j){
        Z_Latent[i] = rtruncnorm(1, a = gamma_update[iteration, j], b = gamma_update[iteration, j + 1], mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
      }
    }
  }
  # Prediction of Y
  gamma_update[iteration,]
  X_beta = Train_X %*% beta_matrix[iteration, ] #Calculations necessary for finding training accuracy
  for(i in 1: length(Train_Y)){
    for(j in 1:K){
      if(X_beta[i] >= gamma_update[iteration,j] && X_beta[i] <= gamma_update[iteration,j + 1]){
        Y_fitted[i] = j # Fitted value of Y_i
      }
    }
  }
  print(paste("Progress %: ", 100 * (iteration/nIter)))
  Train_Accuracy[iteration] = (sum(Y_fitted == Train_Y)/length(Train_Y)) * 100# check
}
# Output to return
estimates = colMeans(beta_matrix[(burn_in+1):nIter, ])
gamma_estimates = colMeans(gamma_update[(burn_in+1):nIter, ])
return(list(estimates = estimates, gamma_estimates = gamma_estimates,Train_Accuracy = Train_Accuracy,beta_matrix = beta_matrix,gamma_update = gamma_update))
}

