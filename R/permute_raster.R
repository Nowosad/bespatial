#' Permute values in the input raster
#'
#' @param x SpatRaster object ([terra::rast()]) containing one or more rasters
#' @param nr_of_permutations Number of permutations performed on each input raster
#'
#' @return A list of matrices
permute_raster = function(x, nr_of_permutations){
    all_vals = terra::values(x, mat = FALSE)
    all_vals_permuted = replicate(nr_of_permutations, sample(all_vals), simplify = FALSE)
    all_vals_permuted = lapply(all_vals_permuted, matrix,
                               ncol = ncol(x), nrow = nrow(x))
    all_vals_permuted
}
