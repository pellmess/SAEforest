#include <Rcpp.h>

#// [[Rcpp::export]]
#Rcpp::NumericVector Quantile(Rcpp::NumericVector x, Rcpp::NumericVector probs) {
#  // implementation of type 7
#  const size_t n=x.size(), np=probs.size();
#  if (n==0) return x;
#  if (np==0) return probs;
#  Rcpp::NumericVector index = (n-1.)*probs, y=x.sort(), x_hi(np), qs(np);
#  Rcpp::NumericVector lo = Rcpp::floor(index), hi = Rcpp::ceiling(index);

#  for (size_t i=0; i<np; ++i) {
#    qs[i] = y[lo[i]];
#    x_hi[i] = y[hi[i]];
#    if ((index[i]>lo[i]) && (x_hi[i] != qs[i])) {
#      double h;
#      h = index[i]-lo[i];
#      qs[i] = (1.-h)*qs[i] + h*x_hi[i];
#    }
#  }
#  return qs;
#}

#/*** R
#N <- 100
#P <- 10
#set.seed(21)
#x <- matrix(runif(N/P), nrow=N/P, ncol=P)
#A <- quantile(x, (19:21)/100)  ## R
#B <- Quantile(x, (19:21)/100)  ## Rcpp
#all(A==B)
#*/
