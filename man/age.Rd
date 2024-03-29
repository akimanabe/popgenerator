% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age.R
\name{age}
\alias{age}
\title{Generate random age}
\usage{
age(n, mean, sd, roundage = FALSE)
}
\arguments{
\item{n}{integer, number of age generated}

\item{mean}{positive numeric}

\item{sd}{positive numeric}

\item{roundage}{round age or allow dicimals. The function will return with ceiling.}
}
\value{
vector
}
\description{
Generate random age
}
\examples{
age(n=100, mean=2, sd=1, roundage=TRUE)
age(n=10, mean=3, sd=1.5)
}
