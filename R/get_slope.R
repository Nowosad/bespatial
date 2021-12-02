#' Calculate a slope
#'
#' @param x A matrix
#' @param neighbourhood The number of directions in which cell adjacencies are
#'   considered as neighbours: 4 (rook's case), 8 (queen's case) or a binary matrix
#'   where the ones define the neighbourhood. The default is 4.
#'
#' @details "Slope" is calculated as follows:
#'   1. For each cell, the algorithm looks at its 4 neighbors and
#'   calculates the absolute difference between the main cell and its neighbors.
#'   2. Next, it sums these four values.
#'   3. After repeating this operation for every cell, it calculates
#'   an average of the sum of the absolute differences for the whole raster.
#'
#' @return A slope value
get_slope = function(x, neighbourhood = matrix(4)){
  mean(cslope(x, directions = neighbourhood))
}
