% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mydiag.panel.R
\name{mydiag.panel}
\alias{mydiag.panel}
\title{Function for making fancy correlation matrices, adapted from Rebecca}
\usage{
mydiag.panel(x, labels, ...)
}
\description{
Function for making fancy correlation matrices, adapted from Rebecca
}
\examples{
pairs(~PC1 + PC2, data = df, lower.panel=twolines, diag.panel=mydiag.panel, upper.panel=panel.cor, label.pos=0.5, main="Correlation matrix of PCs and covariates", labels = c('PC1', 'PC2'))
}
