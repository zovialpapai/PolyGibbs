R Package: PolyGibbs
================
Abhisek Chakraborty

  - [PolyGibbs: Bayesian Analysis of Binary &
Polychotomous Response Data](#polygibbs-bayesian-analysis-of-binary-&-polychotomous-response-data)
  - [Installation](#installation)
  - [Usage](#usage)
      - [1. Coordinates Conversion to New
        CRS](#coordinates-conversion-to-new-crs)
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

### 1\. Coordinates Conversion to New CRS

Conversion of the GPS coordinates into a new CRS is the base to initiate
any GPS analysis studies. `convert_crs` takes the initial latitude and
longitude of a list of points and convert to any new CRS. The default
value for the final projection is `"+proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0"`, which the CRS corresponding to
OSM data (and the CRS for the `SampleTransGPS` data).

``` r
# convert from the initial CRS projection to a new one
convlist <- convert_crs(LatList = LatList, LongList = LongList, InitProj = InitProj, 
    NextProj = NextProj)
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
