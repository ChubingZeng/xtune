<!-- README.md is generated from README.Rmd. Please edit that file -->

:sparkles: ipreg: Penalized regression with differential shrinkage integrating external information <img src="man/figures/logo.png" align="right" />
==================================================================================

[![Build Status](https://travis-ci.org/ChubingZeng/ipreg.svg?branch=master)](https://travis-ci.org/ChubingZeng/ipreg)
[![Coverage status](https://codecov.io/gh/ChubingZeng/ipreg/branch/master/graph/badge.svg)](https://codecov.io/gh/ChubingZeng/ipreg)



&#x1F4D7;  Introduction
=======================

Motivation
----------

Lasso and Ridge regression are very popular techniques for fitting high dimensional data. In their objective function, they have a single penalty parameter *λ* applied equally to all regression coefficients to control the amount of regularization in the model, which could potentially undershrinking important features and overshrinking unimportant features.

The motivation of integrated penalized regression is to improve the prediction accuracy of standard Lasso and Ridge regression by allowing **differential amount of shrinkage** for each regression coefficient by integrating external information. The objective function of integrated penalized regression is: 

![objective](https://user-images.githubusercontent.com/23446412/55191031-5537b280-515e-11e9-89dd-a991275a4a83.png)

The external information could be any nominal or quantiative feature-specific information, such as grouping of predictors, prior knowledge of biological importance, external p-values, function annotations, etc. Each column of Z is a variable for features in X. Z is of dimension *p \times q*, where *p* is the number of features and *q* is the number of variables in Z.

Tuning multiple penalty parameters
----------------------------------

Cross-validation is widely used to tune a single penalty parameter, but it is computationally infeasible to tune more than three penalty parameters. Therefore, we propose an Empirical Bayes approach to learn the multiple tuning parameters. The individual penalties are interpreted as variance terms of the priors (double exponential prior for Lasso and Gaussian prior for Ridge) in a random effect formulation of penalized regressions. A majorization-minimization algorithm is employed for implementation. 

Data structure examples
------------------------
Suppose we want to predict a person's cholesterol level using his/her weekly dietary intake. Our external information Z simply incorporates information about the levels of relevant food constituents in the dietary items.

![example 1](https://user-images.githubusercontent.com/23446412/56444955-da227180-62af-11e9-993d-70feb769e910.png)


&#x1F4D9;  Installation
=======================
*ipreg* can be installed from Github:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("ChubingZeng/ipreg")

library(classo)
```

&#x1F4D8;  Examples
===================
Here are some basic examples on how to use this package:


#### LASSO with differential amount of penalties
``` r

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
