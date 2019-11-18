R Package: PolyGibbs
================
Abhisek Chakraborty

  - [PolyGibbs: Bayesian Analysis of Binary &
Polychotomous Response Data](#polygibbs-bayesian-analysis-of-binary-&-polychotomous-response-data)
  - [Installation](#installation)
  - [Usage](#usage)
      - [1. Data Generation & Fitting Binary Probit Regression Using Gibbs Sampler Algorithm](#data-generation-&-fitting-binary-probit-regression-using-gibbs-sampler-algorithm)
      - [2. Coordinates Interpolation](#coordinates-interpolation)
      - [3. Generate Bounding Boxes](#generate-bounding-boxes)
      - [4. Plot Link Speed and Bounding
        Boxes](#plot-link-speed-and-bounding-boxes)
      - [5. Matching The OSM Link IDs to GPS
        Coordinates](#matching-the-osm-link-ids-to-gps-coordinates)
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
to demonstrate the utility of the package functionalities. 
Then we use BinaryGibbs_fit function to implement Probit Regression for Binary Responses via data augmentation and Gibbs sampling.

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

### 2\. Coordinates Interpolation

Intepolation of coordinates over a desired time sequence is so useful in
case of irregular time sequence or better visualization of data on
contour maps. Function `interpolate_coords` takes the lists of latitudes
and longitudes over an irregular time sequence and interpolates
coordinates.

``` r
# interpolate the GPS coordinates over 3 seconds
interp_data <- interpolate_coords(LatList = LatList, LongList = LongList, 
    timeseq = timeseq, timeint = 3)
```

### 3\. Generate Bounding Boxes

While working with specific few roads rather than the whole network,
generating regions and boxes around the corridors helps to better focus
on the route, instead of the whole area, and remove unnecessary areas or
roads from the data. So, accessing the OSM data can be easier, less
challenging, and can be done in small chunks. `get_boxes` function
splits the corridors in smaller regions by considering a maximum route
distance of `resolution` within each box. `offLong` and `offLat`are the
margins of each box from the closest coordinate point. The output gives
the ID of box (`boxcuts`) for each point as well as the coordinates of
each box (`boxlists`).

``` r
# create bounding boxes
boxout <- get_boxes(LatList = LatList, LongList = LongList, timeseq = timeseq, 
    resolution = resolution, offLong = offLong, offLat = offLat)
boxcuts <- boxout$boxtable$boxcuts
boxlist <- boxout$boxlist
```

### 4\. Plot Link Speed and Bounding Boxes

To better study the bounding boxes generated and estimate the
`resolution` and margins, `plot_route` function is provided. It also has
the option to show the average speed of the vehicle on the corridor.

``` r
#plot the link speeds and bounding boxes
plot_route(LatList=LatList,LongList=LongList,timeseq=timeseq,
           boxlist=boxlist)
```

### 5\. Matching The OSM Link IDs to GPS Coordinates

The process of matching the GPS points to the OSM data links or features
can so challenging and inaccurate. The function `match_highway` in this
package helps with this process in multiple ways. Firstly, it splits the
network in few boxes and reduce the tension of accessing the whole OSM
data at once or setting up a server for “planet.osm” data. Secondly, it
considers `k` close points rather than the closest point to the
coordinate. Finally, it tries to keep the route choice consistency.

``` r
# match each points to an OSM highway
IDList <- match_highway(LatList = LatList, LongList = LongList, timeseq = timeseq, 
    k = 5, boxcuts = boxcuts, boxlist = boxlist)
```

## Details

For more information on TransGPS Package, please access the package
documentations or
[vignettes (NEED TO ADD THE LINK)](). Please feel
free to contact the author.
