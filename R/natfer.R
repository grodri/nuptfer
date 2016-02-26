#' Henry's Natural Fertility Schedule
#'
#' Computes Henry's natural fertility schedule by linear interpolation
#' @param age is a vector of exact ages
#' @export

natfer <- function(age) {
  # Natural fertility
  x <- (11:50) + 0.5
  y <-   c(  0, .175, .225, .275, .325, .375, .421, .46,  .475,
    .477, .475, .47,  .465, .46,  .455, .449, .442, .435, .428,
    .42,  .41,  .4,   .389, .375, .36,  .343, .325, .305, .28,
    .247, .207, .167, .126, .087, .055, .035, .021, .011, .003, 0)
  approx(x, y, xout = age, yleft = 0, yright = 0)$y
}
