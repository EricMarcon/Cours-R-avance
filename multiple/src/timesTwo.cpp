#include <Rcpp.h>
using namespace Rcpp;

//' timesTwo
//'
//' Multiplies by 2
//'
//' @param x An integer
//' @export
// [[Rcpp::export]]
int timesTwo(int x) {
  return x * 2;
}
