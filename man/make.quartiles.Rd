% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.quartiles.R
\name{make.quartiles}
\alias{make.quartiles}
\title{Function to make quartiles from a column within a dataframe --> taken from fashaR}
\usage{
make.quartiles(test)
}
\arguments{
\item{test}{data that needs to be divided into quartiles}
}
\value{
output Dataframe with quartiles (categories and by number, 4 = highest quartile)
}
\description{
Function to make quartiles from a column within a dataframe --> taken from fashaR
}
\examples{
test2=makequartiles(test$resid.mtDNA)
}
