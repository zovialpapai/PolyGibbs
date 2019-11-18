R Package: PolyGibbs
================
Abhisek Chakraborty

  - [PolyGibbs: Bayesian Analysis of Binary &
Polychotomous Response Data](#polygibbs-bayesian-analysis-of-binary-&-polychotomous-response-data)
  - [Installation](#installation)
  - [Usage](#usage)
      - [1. Data Generation & Fitting Binary Probit Regression Using Gibbs Sampler Algorithm](#data-generation-&-fitting-binary-probit-regression-using-gibbs-sampler-algorithm)
      - [2. Prediction via fitted Bayesian Probit Model on Test Set](#prediction-via-fitted-bayesian-probit-model-on-test-set)
      - [3. Calculation of Test Set Accuracy](#calculation-of-test-set-accuracy)
      - [4. Analysis of Convergence of the chain via Traceplots](#analysis-of-convergence-of-the-chain-via-traceplots)
      - [5. Ploting  Posterior Distributions of the Estimated Model Parameters](ploting-posterior-distributions-of-the-estimated-model-parameters)
  - [Details](#details)

## PolyGibbs: Bayesian Analysis of Binary & Polychotomous Response Data

The primary objective of the R Package
is implementation of Probit Regression for Binary Responses
via data augmentation and Gibbs sampling
and its extention in modelling Ordered Multinomial
responses, as discussed in ”Bayesian Analysis of Binary
and Polychotomous Response Data” [Author(s): James
H. Albert and Siddhartha Chib, Source: Journal of the
American Statistical Association]

Probit Regression is used to model ordinal binary
responses with both continuous and categorical
variables as covariates. In data augmentation
technique, given the binary responses we generate
values from a underlying latent normal distribution
and prior structures are assumed on the model
parameters. Next the posterior distribution of the
model parameters are derived and parameters estimation
is carried out by a Gibbs Sampler algorithm
using the full conditional distributions. The
technique can be extended to model multinomial
ordinal responses as well where the cut-off points
on the latent variable axis also become model
parameters and are updated at each iteration of the
Gibbs Sampler algorithm.
An attempt is made to put these two Gibbs
Sampler algorithms in a R Package, along with
the diagnostics plots i.e trace-plots to study the
convergences and density plots of the posterior
distributions. The flexibility of choices on the prior
specification, number of iterations, burn in periods
etc is be maintained.

In biomedical sciences, finance, astronomy etc
researchers and practitioners often face problems
of classifying objects of interest in two or more
ordinal classes. In Wireless sensor networks it’s
often more energy efficient to observe ordinal
categorical or semi-continuous responses rather
than recording continuous responses and data augmentation
technique is often used to estimate the
model parameters. The R package is envisioned to
serve the purpose of modelling in such scenarios.

## Installation

``` r
devtools::install_github("zovialpapai/PolyGibbs")
```

## Usage

``` r
library(PolyGibbs)
```

### 1\.  Data Generation & Fitting Binary Probit Regression Using Gibbs Sampler Algorithm

First we create a simulated data set of n observations each with a binary response ( 0 or 1) and coressponding vlues on p covariates, 
to demonstrate the utility of the package functionalities. (Please note that ,the data generated in this section will be used throughout this article.)
Then we use "BinaryGibbs_fit" function to implement Probit Regression for Binary Responses via data augmentation and Gibbs sampling.

``` r
# Generating Simulated Data 
set.seed(250)
require(truncnorm)
require(MASS)
N <- 500
x1 <- seq(-1, 1, length.out = N)
x2 <- rnorm(N, 0, 1)
D <- 3
X <- matrix(c(rep(1, N), x1, x2), ncol = D)
true_theta <- c(- 0.5, 3.3, 2)
p <- pnorm(X %*% true_theta)
y <- rbinom(N, 1, p)
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
# Fitting Bayesian Probit Regression
BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )

```
The model can be fitted using other choices of the prior specification on the regression  
parameters changing the function inputs.
### 2\. Prediction via fitted Bayesian Probit Model on Test Set

Once we have fitted the model, we need to test the predictive power of the model on a test set. 
Function "BinaryGibbs_Pred" is used for prediction using  the 
fitted Bayesian Probit Model on the test set.

``` r
# Storing the outputs of the model fitting
Output = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
estimates = Output$estimates
# Prediction using the fitted model
BinaryGibbs_Pred(estimates, Test_X)
```

### 3\. Calculation of Test Set Accuracy
Once we have fitted the Bayesian Probit Regression model and found out the model predictions
on a test set, we want to calculate the test set accuracy comparing the actual and predicted levels 
of the responses on the test set. This will let us comment on the efficacy of the fitted model for the 
purpose of prediction.
BinaryGibbs_Test_Accuracy Calculates Accuracy of Prediction on Test Set for Bayesian Probit Regression.



``` r
# Storing the outputs of the model fitting and predictions
Output = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
estimates = Output$estimates
Predicted_Y = BinaryGibbs_Pred(estimates, Test_X)
# Finding the accucary of prediction on the test set
BinaryGibbs_Test_Accuracy(Predicted_Y, Test_Y)
```

### 4\. Analysis of Convergence of the chain via Traceplots

To study the convergence of the gibbs sampler algorithm we need to draw the traceplot of 
parameter estimates that plots the estimated posterior means of various parameters over all the iterations
of the chain. We should expect the chain to more or less stabilize around the estimated posterior mean as 
we progress through the iterations.
BinaryGibbs_Traceplot generates plots for diagnosis of Parameters estimates by Probit Regression for Binary Responses via data augmentation and Gibbs sampling.

``` r
# Storing the outputs of the model fitting
temp = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
# Ploting the traceplot for Beta_0
BinaryGibbs_Traceplot(beta_matrix = temp$beta_matrix, k = 0)

```
The plots can be generated for other regression parameters changing the function inputs.
### 5\. Ploting  Posterior Distributions of the Estimated Model Parameters
Studying the posterior distribution of the estimated model parameters is neccesary to observe the dispersion of the 
drawn values around estimated posterior mean.
BinaryGibbs_PosteriorDistribution_plot Plots Posterior Frequency Distribution of Parameters estimated by Probit Regression for Binary Responses via data augmentation and Gibbs sampling.


``` r
# Storing the outputs of the model fitting
temp = BinaryGibbs_fit(Train_X, Train_Y, nIter, prior, burn_in, prior_mean, prior_var )
# Ploting the Posterior Distribution for Beta_0
BinaryGibbs_PosteriorDistribution_plot(beta_matrix = temp$beta_matrix , k = 0, burn_in = 2500, breaks= 50)

```
The plots can be generated for other regression parameters changing the function inputs.
## Details

For more information on PolyGibbs Package, please access the package
documentations or
[vignettes (NEED TO ADD THE LINK)](). Please feel
free to contact the author.