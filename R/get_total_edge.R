#' Calculate total edge based on the input matrix
#'
#' @param x A matrix
#' @param resolution A numeric vector with two values representing the input matrix
#'   resolution on the x and y axis
#' @param neighbourhood The number of directions in which cell adjacencies are
#'   considered as neighbours: 4 (rook's case), 8 (queen's case) or a binary matrix
#'   where the ones define the neighbourhood. The default is 4.
#'
#' @return A total edge value
get_total_edge = function(x, resolution, neighbourhood = as.matrix(4)){
  resolution_x = resolution[[1]]
  resolution_y = resolution[[2]]
  neighbor_matrix = comat::get_coma(x, neighbourhood = neighbourhood)
  edge_total = sum(neighbor_matrix[lower.tri(neighbor_matrix)]) * resolution_x
  return(edge_total)
}
