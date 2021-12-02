#include <comat.h>
// [[Rcpp::depends(comat)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// @export
// [[Rcpp::export]]
NumericMatrix cslope(IntegerMatrix x,
                     const arma::imat directions) {
  const int na = NA_INTEGER;
  const unsigned ncols = x.ncol();
  const unsigned nrows = x.nrow();

  NumericMatrix result(nrows, ncols);

  // create neighbors coordinates
  IntegerMatrix dirs = 4;
  IntegerMatrix tmp = comat::create_neighborhood(directions);
  int neigh_len = tmp.nrow();
  std::vector<std::vector<int> > neig_coords;
  for (int row = 0; row < neigh_len; row++) {
    IntegerVector a = tmp.row(row);
    std::vector<int> b(a.begin(), a.end());
    neig_coords.push_back(b);
  }

  for (unsigned col = 0; col < ncols; col++) {
    for (unsigned row = 0; row < nrows; row++) {
      const int focal = x[col * nrows + row];
      if (focal == na)
        continue;
      double diff = 0;
      int n = 0;
      for (int h = 0; h < neigh_len; h++) {
        int neig_col = neig_coords[h][0] + col;
        int neig_row = neig_coords[h][1] + row;
        if (neig_col >= 0 &&
            neig_row >= 0 &&
            neig_col < ncols &&
            neig_row < nrows) {
          const int neigh = x[neig_col * nrows + neig_row];
          if (neigh == na)
            continue;
          diff = diff + std::abs(focal - neigh);
          // n = n + 1;
        }
      }
      // result(row,col) = result(row,col) + (diff/n);
      result(row,col) = result(row,col) + diff;

    }
  }
  return result;
}

/*** R
m = matrix(c(50, 45, 50, 30, 30, 30, 8, 10, 10), nrow = 3)
cslope(m, directions = matrix(4))
*/
