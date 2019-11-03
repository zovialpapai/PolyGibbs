# Model Fitting -----------------------------------------------------------
#Libraries required
install.packages("MASS")
install.packages("truncnorm")
require(MASS)
require(truncnorm)
# Train_X: n X p matrix of continuous covarites of training set.
# Train_Y: n X 1 vector of responses of training set. (takes values 0 or 1)
# nIter: An  integer, No of iterations for the Gibbs Sampler
# prior: choice of prior (1: diffuse prior,2: proper conjugate prior)
# burn_in: An integer, No of iterations neglected at begining of the chain in calculation of posterior mean
# prior_mean: a (p+1) X 1 vector specifying mean of the normal conjugate prior, if prior = 2
# prior_var: a (p+1) X (p+1) matrix specifying variance of the normal conjugate prior, if prior = 2

# Inputs: Train_X, Test_X(Not Used), Train_Y, Test_Y(Not Used), nIter, prior, beta_start(Not Used), burn_in, prior_mean, prior_var

#' Title
#'
#' @param Train_X
#' @param Train_Y
#' @param nIter
#' @param prior
#' @param burn_in
#' @param prior_mean
#' @param prior_var
#'
#' @return
#' @export
#'
#' @examples
BinaryGibbs_fit <- function(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var ){
  # To include libraries***
  # beta_start can be taken as the MLE in Prior 1
  # choose prior = 1 to use diffuse prior, prior = 2 to use proper conjugate prior (to be taken as input, worning: its redundant for Prior = 1)
  # Look at where Train_X become  a vector
  # if burn_in +1 == nIter, fails
  # nIter , burn_in can be given as no integer

  #Determining Data Dimensions
  n = nrow(Train_X) # No of observations in trainging data covariates
  p = ncol(Train_X) # No of Covariate Variables

  #Doing Compatibility Checks on the Inputs
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
  # # Checking if Test_X and Test_Y are compatible
  # if(nrow(Test_X) != length(Test_Y) ){
  #   stop("TestX and TestY are incompatible: Dimensions do not match")
  # }
  # # Checking if Test and Train Data are compatible (via no of covariates involved)
  # if(ncol(Test_X) != ncol(Train_X) ){
  #   stop("TestX and TrainX are incompatible: Dimensions do not match")
  # }
  # Checking if both classes are represented in TrainY
  if(length(unique(Train_Y)) != 2 ){
    stop("Must have observations from both classes in TrainY")
  }
  # Checking if classes in TrainY are represented as 0 or 1 .
  if(min(Train_Y) != 0 || max(Train_Y) != 1 ){
    stop("Indexing in TrainY must be 0 and 1")
  }

  # Including intercept into the model
  Train_X = cbind(rep(1, nrow(Train_X)), Train_X)

  #Declaring variables used in Gibbs Sampling
  beta_matrix = matrix(NA, nrow = nIter, ncol = (p + 1) ) # Storage for beta updates over iterations
  Z_Latent = vector(length = n) # Vector to store latent variables
  Y_fitted = vector(length = n) # Vector to store fitted values of y
  Train_Accuracy = vector(length = nIter) # Vector to store Percentage Accuracy

  #With diffused prior
  if(prior == 1){
    # Warning for unused arguments
    if(!is.null(prior_mean) | !is.null(prior_var)){
      warning("Unused Arguments: prior_mean & prior_var")}
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
    # Initial Generation of latent varible Z based on Train_Y
    for(i in 1: length(Train_Y)){
      if(Train_Y[i] == 1) {
        mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_start))) # Determing mean of the Latent varible Zi
        Z_Latent[i] = rtruncnorm(1, a = 0, b = Inf, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
      }else{
        mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_start))) # Determing mean of the Latent varible Zi
        Z_Latent[i] = rtruncnorm(1, a = -Inf, b = 0, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
      }
    }

    # Iterations of the algorithm
    iteration = 2
    for( iteration in 2:nIter){
      # Updating beta using full conditional of beta given Z, Train_Y
      mean_beta_iter_i = kernel_inverse %*% (t(as.matrix(Train_X)) %*% as.matrix(Z_Latent))
      beta_matrix[iteration, ] = mvrnorm(1, mean_beta_iter_i,  kernel_inverse)
      # Updating Latent variables Zi using full conditionals
      for(i in 1: length(Train_Y)){
        if(Train_Y[i] == 1) {
          mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_matrix[iteration, ]))) # Determing mean of the Latent varible Zi
          Z_Latent[i] = rtruncnorm(1, a = 0, b = Inf, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
          Y_fitted[i] = rbinom(n = 1, size = 1, prob = pnorm(mean_TrainX_i, mean = 0, sd = 1)) # Fitted value of Y_i
        }else{
          mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_matrix[iteration, ]))) # Determing mean of the Latent varible Zi
          Z_Latent[i] = rtruncnorm(1, a = -Inf, b = 0, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
          Y_fitted[i] = rbinom(n = 1, size = 1, prob = pnorm(mean_TrainX_i, mean = 0, sd = 1)) # Fitted value of Y_i
        }
      }
      print(paste("Progress %: ", 100 * (iteration/nIter)))
      Train_Accuracy[iteration] = sum(Y_fitted == Train_Y)/length(Train_Y)
    }
    # Output to return
    # colMeans(beta_matrix[(burn_in+1):nIter, ])
    # Train_Accuracy
    # Include Trace Plots, Histogram, comparison with mle
    # Testing
  }

  #With proper conjugate prior
  if(prior == 2) {
    # compatibilty checks on prior specification
    # Check compatibillity of prior_mean
    if(nrow(as.matrix(prior_mean)) != (p +1)){
      stop("prior_mean is not compatible")
    }
    # Check Compatibility of Prior_Var
    if(nrow(as.matrix(prior_var)) != (p +1) || ncol(as.matrix(prior_var)) != (p +1) ){
      stop("prior_var is not compatible")
    }
    #Check if prior_var is computationally singular
    if(abs(det(prior_var)) < 0.001 ){
      stop("prior_var is computationally singular")
    }
    prior_var_inverse = solve(prior_var)# Inverting prior_var
    kernel_modified = solve(prior_var) + (t(as.matrix(Train_X)) %*% as.matrix(Train_X))# A matrix of importance
    kernel_modified_inverse = solve(kernel_modified) # inverting kernel_modified
    #Check if kernel_modified is computationally singular
    if(abs(det(kernel_modified)) < 0.001 ){
      stop("solve(prior_var) + (t((Train_X)) %*% (Train_X)) is computationally singular")
    }
    #beta_start = (kernel_inverse %*% (t(as.matrix(Train_X)) %*% as.vector(Train_Y))) # Calculating initial beta
    beta_start = rep(0, (p+1))
    beta_matrix[1, ] = beta_start # Storing initial beta
    # Initial Generation of latent varible Z based on Train_Y
    for(i in 1: length(Train_Y)){
      if(Train_Y[i] == 1) {
        mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_start))) # Determing mean of the Latent varible Zi
        Z_Latent[i] = rtruncnorm(1, a = 0, b = Inf, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
      }else{
        mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_start))) # Determing mean of the Latent varible Zi
        Z_Latent[i] = rtruncnorm(1, a = -Inf, b = 0, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
      }
    }
    # Iterations of the algorithm
    iteration = 2
    for( iteration in 2:nIter){
      # Updating beta using full conditional of beta given Z, Train_Y
      mean_beta_iter_i = kernel_modified_inverse %*% ((prior_var_inverse %*% prior_mean) + (t(as.matrix(Train_X)) %*% as.matrix(Z_Latent)))
      beta_matrix[iteration, ] = mvrnorm(1, mean_beta_iter_i,  kernel_modified_inverse)
      # Updating Latent variables Zi using full conditionals
      for(i in 1: length(Train_Y)){
        if(Train_Y[i] == 1) {
          mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_matrix[iteration, ]))) # Determing mean of the Latent varible Zi
          Z_Latent[i] = rtruncnorm(1, a = 0, b = Inf, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
          Y_fitted[i] = rbinom(n = 1, size = 1, prob = pnorm(mean_TrainX_i, mean = 0, sd = 1)) # Fitted value of Y_i
        }else{
          mean_TrainX_i = sum((as.vector(Train_X[i, ])) * (as.vector(beta_matrix[iteration, ]))) # Determing mean of the Latent varible Zi
          Z_Latent[i] = rtruncnorm(1, a = -Inf, b = 0, mean = mean_TrainX_i, sd = 1) # Generating from Latent variable
          Y_fitted[i] = rbinom(n = 1, size = 1, prob = pnorm(mean_TrainX_i, mean = 0, sd = 1)) # Fitted value of Y_i
        }
      }
      print(paste("Progress %: ", 100 * (iteration/nIter)))
      Train_Accuracy[iteration] = sum(Y_fitted == Train_Y)/length(Train_Y)
    }
    # Output to return
    # colMeans(beta_matrix[(burn_in+1):nIter, ])
    # Train_Accuracy
    # Include Trace Plots, Histogram, comparison with mle
    # Testing
  }
  # Calculating posterior means
  estimates = colMeans(beta_matrix[(burn_in+1):nIter, ])
  # Return Output
  # beta_matrix: a nIter X (p+1) matrix of beta estimate chains
  # estimates: a (p+1) X 1 vector of beta estimates
  # Train_Accuracy: a nIter X 1 vector of accuracy

  return(list(beta_matrix = beta_matrix, estimates = estimates,Train_Accuracy = Train_Accuracy ))
}
