#include <Rcpp.h>
using namespace Rcpp;

// This next line is all it takes to find the bigmemory 
// headers -- thanks to the magic of Rcpp attributes.
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>

#include <numeric>

// Logic for BigColSums.
template <typename T>
NumericVector BigColSums(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat) {
    // Create the vector we'll store the column sums in.
    NumericVector colSums(pMat->ncol());
    for (unsigned int i=0; i < pMat->ncol(); ++i)
        colSums[i] = std::accumulate(mat[i], mat[i]+pMat->nrow(), 0.0);
    return colSums;
}

//' Dispatch function for BigColSums
//' @param pBigMat SEXP container for the external pointer to the BigMatrix
//'   object
//' @useDynLib BigMatrixRcppTest
//' @import bigmemory
//' @import BH
//' @importFrom Rcpp evalCpp
//' @export
// [[Rcpp::export]]
NumericVector BigColSums(SEXP pBigMat) {
    // First we have to tell Rcpp what class to use for big.matrix objects.
    // This object stores the attributes of the big.matrix object passed to it
    // by R.
    XPtr<BigMatrix> xpMat(pBigMat);

    // To access values in the big.matrix, we need to create a MatrixAccessor
    // object of the appropriate type. Note that in every case we are still
    // returning a NumericVector: this is because big.matrix objects only store
    // numeric values in R, even if their type is set to 'char'. The types
    // simply correspond to the number of bytes used for each element.
    switch(xpMat->matrix_type()) {
      case 1:
        return BigColSums(xpMat, MatrixAccessor<char>(*xpMat));
      case 2:
        return BigColSums(xpMat, MatrixAccessor<short>(*xpMat));
      case 4:
        return BigColSums(xpMat, MatrixAccessor<int>(*xpMat));
      case 8:
        return BigColSums(xpMat, MatrixAccessor<double>(*xpMat));
      default:
        // This case should never be encountered unless the implementation of
        // big.matrix changes, but is necessary to implement shut up compiler
        // warnings.
        throw Rcpp::exception("unknown type detected for big.matrix object!");
    }
}
