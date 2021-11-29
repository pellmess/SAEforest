#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function.

NumericVector Quantile(NumericVector x, NumericVector probs) {
  // implementation of type 7
  const size_t n=x.size(), np=probs.size();
  if (n==0) return x;
  if (np==0) return probs;
  NumericVector index = (n-1.)*probs, y=x.sort(), x_hi(np), qs(np);
  NumericVector lo = floor(index), hi = ceiling(index);

  for (size_t i=0; i<np; ++i) {
    qs[i] = y[lo[i]];
    x_hi[i] = y[hi[i]];
    if ((index[i]>lo[i]) && (x_hi[i] != qs[i])) {
      double h;
      h = index[i]-lo[i];
      qs[i] = (1.-h)*qs[i] + h*x_hi[i];
    }
  }
  return qs;
}


NumericVector Gini(NumericVector x) {
  double n = x.size();
  NumericVector retval = 0, term1 =0, term2 =0, term3 =0;
  IntegerVector s1 = seq_len(n);
  NumericVector i = as<NumericVector>(s1);

  term1 = 2 * sum(x * i);
  term2 = n * sum(x);
  term3 = (n + 1) / n;

  retval =  (term1 / term2) - term3;
  return retval;
}



// [[Rcpp::export]]
NumericVector calc_indicatC(NumericVector Y, double threshold, Rcpp::Nullable<Rcpp::IntegerVector> custom = R_NilValue) {
  double hcr = 0;
  NumericVector pgap = 0, pgap2 = 0, qsr1 = 0, qsr2 = 0, gini=0;

  NumericVector quantval = Quantile(Y, {0.1, 0.2,0.25, 0.5, 0.75,0.8, 0.9});
  NumericVector quantex = {0,2,3,4,6};
  CharacterVector namesex = {"Mean","Quant10","Quant25","Median","Quant75","Quant90","Gini","Hcr","Pgap","Qsr"};
  NumericVector retval = quantval[quantex];

  retval.push_front(mean(Y));

  gini = Gini(Y);
  retval.push_back(sum(gini));

  hcr = mean(Y < threshold);
  retval.push_back(hcr);

  pgap = (Y < threshold);
  pgap2 = (threshold - Y) / threshold;

  pgap = pgap * pgap2;
  retval.push_back(mean(pgap));

  qsr1 = Y[Y > quantval[5]];
  qsr2 = Y[Y < quantval[1]];
  retval.push_back(sum(qsr1)/sum(qsr2));

  retval.attr("names") = namesex;

  return retval;
}


NumericVector rep_v( NumericVector x, int n){
  NumericVector res = rep_each(x, n) ;
  return res ;
}

// [[Rcpp::export]]
NumericVector smear_fun(NumericVector popSize, List unit_preds, NumericVector oob_res, double threshold){

  int nd = unit_preds.size(), res_d = oob_res.size();
  NumericMatrix smear_mat(nd , 10 );
  CharacterVector namesex = {"Mean","Quant10","Quant25","Median","Quant75","Quant90","Gini","Hcr","Pgap","Qsr"};

  for (int i=0; i < nd; ++i) {
    NumericVector loop_v = rep_v(oob_res, popSize[i]);
    NumericVector pred_v = rep_v(unit_preds[i], res_d);

    loop_v = loop_v + pred_v;
    smear_mat(i,_) = calc_indicatC(loop_v, threshold); }

  colnames(smear_mat) = namesex;

  return smear_mat;
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// /*** R
// calc_indicatC(Y=1:100, threshold =50)
//
// popSize <- c(100,100)
// oob_res <- runif(100)
// unit_preds <- list(a = runif(100,1,100), b= runif(100,1,100))
// threshold <- 50
//
//  smear_fun(popSize, unit_preds, oob_res, threshold)
// */


// library(microbenchmark)
// sourceCpp("C:/Users/Patrick/Desktop/test_file_indicat.cpp")

//   microbenchmark(calc_indicatC(c(smear_i), threshold = thresh), calc_indicat(c(smear_i), threshold = thresh,
//                                custom = NULL))

// #Unit: milliseconds
// #expr     min       lq     mean   median       uq
// #calc_indicatC(c(smear_i), threshold = thresh) 16.9226 18.25890 18.85226 18.75640 19.32360
// #calc_indicat(c(smear_i), threshold = thresh, custom = NULL) 17.4998 21.10845 25.39297 22.02785 22.98905
// #max neval
// #21.5450   100
// #87.7948   100

