<!-- README.md is generated from README.Rmd. Please edit that file -->
classo: R package for *L*<sub>1</sub> and *L*<sub>2</sub> penalized regression with differential penalties
==================================================================================
[![Build Status](https://travis-ci.org/ChubingZeng/classo.svg?branch=master)](https://travis-ci.org/ChubingZeng/classo)

&#x1F4D7;  Introduction
------------

Standard *L*<sub>1</sub> penalized regression (LASSO) and *L*<sub>2</sub> penalized regression (Ridge) have one global penalty parameter *λ* that controls the amount of shrinakge for all predictors, which could potentially undershrinking important features and overshrinking unimportant features.

The aim of *classo* is to allow **differential amount of shrinkage** for each regression coefficients for *L*<sub>1</sub> and *L*<sub>2</sub> penalized regression. The amount of shrinkage for each predictor is modeled as a function of the prior knowledge *Z* provided into the model.

![objective](https://user-images.githubusercontent.com/23446412/55191031-5537b280-515e-11e9-89dd-a991275a4a83.png)


This package has two main functions for now.

-   Empirical Bayes (EB) tuning
    EB tuning is a novel way to find the tuning parameter for LASSO, as an alternative to cross-validation.

$$ min\_{\\beta}\\frac{1}{2} \\sum\_{i=1}^{i=p}(Y\_i - \\beta\_0 - \\sum\_{j=1}^{j=p}X\_{ij}\\beta\_j)^2 + \\lambda \\sum\_{j=1}^{j=p}|\\beta\_j| $$

-   Customized LASSO
    Customized LASSO extends standard LASSO to allow differential penalization on coefficients. You can choose to plug in external data which contains information about the predictors. For example, Gene Ontology annotations or previous study restults. Please note that the number of rows in the external data should match the number of columns in your design matrix.

$$ min\_{\\beta}\\frac{1}{2} \\sum\_{i=1}^{i=p}(Y\_i - \\beta\_0 - \\sum\_{j=1}^{j=p}X\_{ij}\\beta\_j)^2 + \\sum\_{j=1}^{j=p} \\lambda(Z\_j)|\\beta\_j| $$

Currently, we allow both continuous and binary outcomes. The binary outcome case is based on the extension of linear outcome case using linear discriminat analysis.

&#x1F4D9;  Installation
------------

You can install *classo* from github with:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("ChubingZeng/classo")

library(classo)
```

Example
-------

### Empirical Bayes Tuning

### LASSO with custom penalization

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```
