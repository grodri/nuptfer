#' Coale's Marital Fertility Model
#'
#' Computes model marital fertility rates by age
#' @param age vector of exact ages
#' @param  level (M) scalar representing natural fertility level, default 1
#' @param  control (m) scalar representing degree of control, default 0
#' @export

marfer <- function(age, level=1, control=0) {
  if(level <= 0) {
    stop("Level of natural fertility (M) must be positive")
  }
  if(control < 0) {
    stop("Contol parameter (m) must be non-negative")
  }
  level * natfer(age) * exp( -control*confer(age))
}
