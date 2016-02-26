#' The Coale-McNeil Nuptiality Model
#'
#' Computes cumulative probabilities of marriage by age
#' @param age vector of exact ages
#' @param mean scalar representing mean age at marriage
#' @param stdev scalar representing standard deviation of age at marriage
#' @param pem probability of ever marrying, defaulting to 1
#' @export
pnupt <- function(age, mean, stdev, pem=1) {
  if(stdev <= 0)
    stop("Standard deviation must be positive")
  if( pem <= 0 | pem > 1)
    stop ("Probability of ever-marrying must be in (0,1]")
  z <- (age - mean)/stdev
  pem * pgamma( exp(-1.896 * (z + 0.805)), shape=0.604, lower.tail = FALSE)
}

