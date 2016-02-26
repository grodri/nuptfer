#' Hernes Nuptiality Model
#' 
#' Computes cumulative probabilities of marriage by age
#' @param age vector of exact ages
#' @param A scalar representing attractiveness at age 15
#' @param r scalar representing the decay rate, so Hernes's b = exp(-r)
#' @param pem scalar representing the probability of ever marrying

#' @export
phernes <- function(age, A, r, pem) {
  x = exp(-r*(age-15))
  plogis(qlogis(pem) - A * x / r)
}
