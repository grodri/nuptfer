#' The Coale-Trussell Fertility Model
#'
#' Computes model general fertility rates by age
#' @param age vector of exact ages
#' @param level scalar representing level (M * pem)
#' @param control scalar representing degree of control (m)
#' @param mean scalar representing mean age of entry into union
#' @param stdev scalar representing std. dev. of age at union
#' @export
genfer <- function(age, mean, stdev, level, control) {
  if(level <= 0) {
    stop("Level parameter must be non-negative")
  }
  if(mean <= 0) {
    stop("Mean age of entry into union must be no-negative")
  }
  if(stdev <= 0) {
    stop("Standard deviation of age of entry into union must be no-negative")
  }
  marfer(age, level, control) * pnupt(age, mean, stdev, 1)
}

