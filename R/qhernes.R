#' Hernes Nuptiality Model
#' 
#' Computes quantiles of age at marriage
#' @param p vector of cumulative probabilities to compute quantiles
#' @param A scalar representing attractiveness at age 15
#' @param r scalar representing the decay rate, so Hernes's b = exp(-r)
#' @param pem scalar representing the probability of ever marrying
#' @export
qhernes <- function(p, A, r, pem) {
  15 - log(-(qlogis(p) - qlogis(pem))*r/A)/r
}
