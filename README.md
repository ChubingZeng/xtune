<!-- README.md is generated from README.Rmd. Please edit that file -->
classo: R package for *L*<sub>1</sub> and *L*<sub>2</sub> penalized regression with differential penalties
==================================================================================
[![Build Status](https://travis-ci.org/ChubingZeng/classo.svg?branch=master)](https://travis-ci.org/ChubingZeng/classo)
[![Coverage status](https://codecov.io/gh/ChubingZeng/classo/branch/master/graph/badge.svg)](https://codecov.io/github/ChubingZeng/classo?branch=master)

&#x1F4D7;  Introduction
------------

Standard *L*<sub>1</sub> penalized regression (LASSO) and *L*<sub>2</sub> penalized regression (Ridge) have one global penalty parameter *λ* that controls the amount of shrinakge for all predictors, which could potentially undershrinking important features and overshrinking unimportant features.

The aim of *classo* is to allow **differential amount of shrinkage** for each regression coefficients for *L*<sub>1</sub> and *L*<sub>2</sub> penalized regression. The amount of shrinkage for each predictor is modeled as a function of the prior knowledge *Z* provided into the model.

![objective](https://user-images.githubusercontent.com/23446412/55191031-5537b280-515e-11e9-89dd-a991275a4a83.png)

This package has two main functions for now.

-   Customized Penalized Regression: *cus_penalized_reg()* 

    Customized Penalized Regression extends standard LASSO to allow differential penalization on for each regression coefficients. 

-   Empirical Bayes (EB) tuning: *eb_tuning()* 

    EB tuning is a novel way to find the tuning parameter for LASSO and Ridge regression using an empirical Bayes approach. It can be used as an alternative to cross-validation.

Currently, we allow both continuous and binary outcomes. The binary outcome case is based on the extension of linear outcome case using linear discriminat analysis.

&#x1F4D9;  Installation
------------
*classo* can be stored from Github:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("ChubingZeng/classo")

library(classo)
```

&#x1F4D8;  Example
-------
Here are some basic examples on how to use this package:
#### LASSO with differential amount of penalties
``` r
## basic example code
```
#### Ridge with differential amount of penalties
``` r
## basic example code
```
#### Empirical Bayes tuning for LASSO
``` r
## basic example code
```
#### Empirical Bayes tuning for Ridge
``` r
## basic example code
```
