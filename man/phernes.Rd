% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phernes.R
\name{phernes}
\alias{phernes}
\title{Hernes Nuptiality Model}
\usage{
phernes(age, A, r, pem)
}
\arguments{
\item{age}{vector of exact ages}

\item{A}{scalar representing attractiveness at age 15}

\item{r}{scalar representing the decay rate, so Hernes's b = exp(-r)}

\item{pem}{scalar representing the probability of ever marrying}
}
\description{
Computes cumulative probabilities of marriage by age
}

