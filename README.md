<!-- README.md is generated from README.Rmd. Please edit that file -->

:sparkles: ipreg: Penalized regression with differential shrinkage integrating external information <img src="man/figures/logo.png" align="right" />
==================================================================================

[![Build Status](https://travis-ci.org/ChubingZeng/ipreg.svg?branch=master)](https://travis-ci.org/ChubingZeng/ipreg)
[![Coverage status](https://codecov.io/gh/ChubingZeng/ipreg/branch/master/graph/badge.svg)](https://codecov.io/gh/ChubingZeng/ipreg)



&#x1F4D7;  Introduction
=======================

Motivation
----------

Lasso and Ridge regression are very popular techniques for fitting high dimensional data. In their objective function, they have one global penalty parameter *λ* to control the amount of shrinakge for all predictors, which could potentially undershrinking important features and overshrinking unimportant features.

The motivation of integrated penalized regression to improve the prediction accuracy of standard Lasso and Ridge regression by allowing **differential amount of shrinkage** for each regression coefficient based on external information. The external information could be any nominal or quantiative feature-specific information, such as grouping of predictors, prior knowledge of biological importance, external p-values, function annotations, etc. 

![objective](https://user-images.githubusercontent.com/23446412/55191031-5537b280-515e-11e9-89dd-a991275a4a83.png)

Cross-validation is widely used to tune a single penalty parameter, but it is computationally infeasible to tune more than three penalty parameters. Instead, we propose an Empirical Bayes approach to learn the multiple tuning parameters. The individual penalties are interpreted as variance terms of the priors (double exponential prior for Lasso and Gaussian prior for Ridge) in a random effect formulation of penalized regressions. 

Currently, we allow both continuous and binary outcomes. The binary outcome case is based on the extension of linear outcome case using linear discriminat analysis.

&#x1F4D9;  Installation
=======================
*classo* can be stored from Github:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("ChubingZeng/classo")

library(classo)
```

&#x1F4D8;  Examples
===================
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
