#' Coale's Schedule of Fertility Control
#'
#' Computes Coale's schedule of control by age by linear interpolation
#'@param age vector of exact ages

#'@export
confer <- function(age) {
  x <- (19:49) + 0.5
  y <- c(0, .004, .03,  .06,  .1,   .15,  .2,   .25,  .31,  .37,  .44,
            .52,   .6,  .68,  .76,  .83,  .9,   .97, 1.04, 1.11, 1.18,
           1.25, 1.32, 1.39, 1.46, 1.53, 1.59, 1.64, 1.67, 1.69, 1.7)
  approx(x, y, xout = age, yleft = 0, yright = 1.7)$y
}
