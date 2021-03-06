% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfold.R
\name{kfold}
\alias{kfold}
\title{(Un)Stratified k-fold for any type of label}
\usage{
kfold(y, k = 5, stratified = TRUE, seed = 0, named = TRUE)
}
\arguments{
\item{y}{Type: The label vector.}

\item{k}{Type: integer. The amount of folds to create. Causes issues if \code{length(y) < k} (e.g more folds than samples). Defaults to \code{5}.}

\item{stratified}{Type: boolean. Whether the folds should be stratified (keep the same label proportions) or not. Defaults to \code{TRUE}.}

\item{seed}{Type: integer. The seed for the random number generator. Defaults to \code{0}.}

\item{named}{Type: boolean. Whether the folds should be named. Defaults to \code{TRUE}.}
}
\value{
A list of vectors for each fold, where an integer represents the row number.
}
\description{
This function allows to create (un)stratified folds from a label vector.
}
\examples{
# Reproducible Stratified folds
data <- 1:5000
folds1 <- kfold(y = data, k = 5, stratified = TRUE, seed = 111)
folds2 <- kfold(y = data, k = 5, stratified = TRUE, seed = 111)
identical(folds1, folds2)

# Stratified Regression
data <- 1:5000
folds <- kfold(y = data, k = 5, stratified = TRUE)
for (i in 1:length(folds)) {
  print(mean(data[folds[[i]]]))
}

# Stratified Multi-class Classification
data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
folds <- kfold(y = data, k = 5, stratified = TRUE)
for (i in 1:length(folds)) {
  print(mean(data[folds[[i]]]))
}

# Unstratified Regression
data <- 1:5000
folds <- kfold(y = data, k = 5, stratified = FALSE)
for (i in 1:length(folds)) {
  print(mean(data[folds[[i]]]))
}

# Unstratified Multi-class Classification
data <- c(rep(0, 250), rep(1, 250), rep(2, 250))
folds <- kfold(y = data, k = 5, stratified = FALSE)
for (i in 1:length(folds)) {
  print(mean(data[folds[[i]]]))
}

}
