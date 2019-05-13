## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ex1-----------------------------------------------------------------
library(xtune)
data("example")
X <- example$X; Y <- example$Y; Z <- example$Z
dim(X);dim(Z)

## ----dim-----------------------------------------------------------------
X[1:3,1:10]

## ------------------------------------------------------------------------
Z[1:10,]

## ----fit1----------------------------------------------------------------
fit.example1 <- xtune(X,Y,Z)

## ----ex1uni--------------------------------------------------------------
unique(fit.example1$penalty.vector)

## ----ex2_data------------------------------------------------------------
data(diet)
head(diet$DietItems)
head(diet$weightloss)

## ----ex2ex---------------------------------------------------------------
head(diet$NuitritionFact)

## ----exfit---------------------------------------------------------------
fit.diet = xtune(X = diet$DietItems,Y=diet$weightloss,Z = diet$NuitritionFact, family="binary")

## ----indiv---------------------------------------------------------------
fit.diet$penalty.vector

## ----ex3_data------------------------------------------------------------
data(gene)
gene$GeneExpression[1:3,1:5]
gene$PreviousStudy[1:5,]

