#' The Coale-McNeil Nuptiality Model
#'
#' Computes quantiles of age at marriage
#' @param prob vector of probabilities of marrying
#' @param mean scalar representing mean age at marriage
#' @param stdev scalar representing standard deviation of age at marriage
#' @param pem probability of ever marrying, defaulting to 1

#' @export
qnupt <- function(prob, mean, stdev, pem = 1) {
  if(stdev <= 0) {
    stop("Standard deviation must be positive")
  }
  if(pem <= 0 | pem > 1) {
    stop("probability of ever marrying must be in (0,1])")
  }
  if(any(prob <= 0 | prob > pem)) {
    stop("probabilities must be between 0 and pem")
  }
  p <- prob/pem
  g <- qgamma(p, 0.604, lower.tail = FALSE)
  z <- -0.805 - log(g)/1.896
  mean + stdev * z
}
