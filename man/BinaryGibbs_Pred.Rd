% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Binary_Gibbs_Functions_Prediction.R
\name{BinaryGibbs_Pred}
\alias{BinaryGibbs_Pred}
\title{Prediction using fitted Bayesian Probit Model.}
\usage{
BinaryGibbs_Pred(estimates, Test_X)
}
\arguments{
\item{estimates}{a (p+1) X 1 vector of beta estimates, where beta0, beta1, beta2 etc are sorted as 0,1,2...}

\item{Test_X}{a () X p matrix of continuous scale covarites}
}
\value{
Predicted_Y: a nrow(Test_X) X 1 vector of predicted responses
}
\description{
\code{BinaryGibbs_Pred} Predicts using fitted Bayesian Probit Model.
}
\examples{
set.seed(250)
require(truncnorm)
require(MASS)
N <- 500
x1 <- seq(-1, 1, length.out = N)
x2 <- rnorm(N, 0, 1)
D <- 3
X <- matrix(c(rep(1, N), x1, x2), ncol = D)
true_theta <- c(- 0.5, 3.3, 2)
p <- pnorm(X \%*\% true_theta)
y <- rbinom(N, 1, p)
N1  <- sum(y)  # Number of successes
N0  <- N - N1  # Number of failures
#Spliting The Data in Train and Test in 80:20 ratio
Train_ID = sample(1:nrow(X), round(nrow(X) * 0.8), replace = FALSE) # Train Data IDS
Train_X = X[Train_ID, -1] # Train Data Covariates
Test_X = X[-Train_ID, -1] # Test Data Covarites
Train_Y = y[Train_ID] # Train Data Response
Test_Y = y[-Train_ID] # Test Data Response
nIter = 10000
burn_in = round(nIter * 0.5)
prior = 2
prior_mean = rep(1, 3)
prior_var = diag(10, 3)
Output = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
estimates = Output$estimates
BinaryGibbs_Pred(estimates, Test_X)

}
