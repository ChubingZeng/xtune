<!-- README.md is generated from README.Rmd. Please edit that file -->

xtune: Tuning differential shrinkage parameters of penalized regression models based on external information <img src="man/figures/logo.png" align="right" />
=======

[![Build Status](https://travis-ci.org/ChubingZeng/xtune.svg?branch=master)](https://travis-ci.org/ChubingZeng/xtune)
[![Coverage status](https://codecov.io/gh/ChubingZeng/xtune/branch/master/graph/badge.svg)](https://codecov.io/gh/ChubingZeng/xtune)



&#x1F4D7;  Introduction
-----------------------

### Motivation

Lasso and Ridge regression are very popular techniques for fitting high dimensional data. In their objective function, they have a single penalty parameter *λ* applied equally to all regression coefficients to control the amount of regularization in the model, which could potentially undershrinking important features and overshrinking unimportant features.

The motivation of integrated penalized regression is to improve the prediction accuracy of standard Lasso and Ridge regression by allowing **differential amount of shrinkage** for regression coefficients by integrating external information.

The objective function of integrated Lasso regression with differential shrinkage is: 

![lasso](https://user-images.githubusercontent.com/23446412/56463267-9b5eeb00-6385-11e9-8aed-82b9df287d5e.png)

The objective function of integrated Ridge regression with differential shrinkage is: 

![ridge](https://user-images.githubusercontent.com/23446412/56463270-a580e980-6385-11e9-94a9-2a4245127670.png)



The external information could be any nominal or quantiative feature-specific information, such as grouping of predictors, prior knowledge of biological importance, external p-values, function annotations, etc. Each column of Z is a variable for features in design matrix X. Z is of dimension ![equation](https://latex.codecogs.com/gif.latex?p&space;\times&space;q), where *p* is the number of features and *q* is the number of variables in Z.

### Tuning multiple penalty parameters

Cross-validation is widely used to tune a single penalty parameter, but it is computationally infeasible to tune more than three penalty parameters. Therefore, we propose an **Empirical Bayes** approach to learn the multiple tuning parameters. The individual penalties are interpreted as variance terms of the priors (double exponential prior for Lasso and Gaussian prior for Ridge) in a random effect formulation of penalized regressions. A majorization-minimization algorithm is employed for implementation. 

Once the tuning parameters *λs* are estimated, and therefore the penalties known, the regression coefficients are obtained using `glmnet`. 

### Data structure examples

Suppose we want to predict a person's cholesterol level using his/her weekly dietary intake. Our external information Z could incorporates information about the levels of relevant food constituents in the dietary items.

![example](https://user-images.githubusercontent.com/23446412/56463366-969b3680-6387-11e9-8023-a2cc4bc0cd0d.png)


&#x1F4D9;  Installation
-----------------------
`xtune` can be installed from Github using the following command:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("ChubingZeng/xtune")

library(xtune)
```

&#x1F4D8;  Examples
-------------------
To show some examples on how to use this package, we simulated an example data that contains 100 observations, 200 predictors, and an continuous outcome. The external information Z contains 3 columns, each column is indicator variable (can be viewed as the grouping of predictors). 

``` r
## load the example data
data(example)
```

`xtune()` is the core function to fit integrated penalized regression model. At a minimum, you need to specify the predictor matrix `X`, outcome variable `Y`. If an external information matrix `Z` is provided, the function will incorporate `Z` to allow differential shrinkage based on Z. The estimated tuning parameters are returned in `$penalty.vector`. 

If you do not provide external information `Z`, the function will perform empirical Bayes tuning to choose the single penalty parameter in penalized regression, as an alternative to cross-validation. You could compare the tuning parameter choosen by empirical Bayes tuning to that choose by cross-validation (see also `cv.glmnet`). The default penalty applied to the predictors is the Lasso penalty. 

If you provide an identify matrix as external information Z to `xtune()`, the function will estimate a seperate tuning parameter ![equation](https://latex.codecogs.com/gif.latex?\lambda_j) for each regression coefficient ![equation](https://latex.codecogs.com/gif.latex?\beta_j).

``` r
xtune.fit <- xtune(X,Y,Z)

## for ridge
## xtune.fit <- xtune(X,Y,Z,method = "ridge")
```

To view the penalty parameters estimated by `xtune()`

```
xtune.fit$penalty.vectors
```

The `coef` and `predict` functions can be used to extract beta coefficient estimates and predict response on new data. 

``` r
coef(xtune)
predict(xtune.fit, X)
```

