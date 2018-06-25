#' Calculates euclidean distance between 2D data points
#' 
#' @param x A number.
#' @param y A number.
#' @return distance: euclidean distance between points, real
#' @examples
#' CalculateDistance(c(1,2), c(3,4))

CalculateDistance <- function(x, y) {
  # Calculates euclidean distance between 2D data points
  #
  # Args:
  #    x: first 2D point (number)
  #    y: second 2D point (number) 
  #
  # Returns:
  #    distance: euclidean distance between points, real
  sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)
}