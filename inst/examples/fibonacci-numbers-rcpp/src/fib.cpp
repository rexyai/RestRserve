#include<Rcpp.h>

// [[Rcpp::export]]
int calc_fib_cpp(const int n) {
  if (n < 1) return(0);
  int old = 0;
  int cur = 1;
  int hold;
  for (int i = 1; i < n; ++i) {
    hold = cur;
    cur += old;
    old = hold;
  }
  return cur;
}
