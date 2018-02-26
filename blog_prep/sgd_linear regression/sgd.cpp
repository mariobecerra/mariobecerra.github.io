#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <iostream>
using namespace std;
using namespace Rcpp;
using namespace RcppArmadillo;

// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
NumericVector gradient(NumericMatrix data, NumericVector betas){
  int n = data.nrow();
  int p = data.ncol();
  NumericVector li(n);
  NumericVector g_out(p);
  for(int i = 0; i < n; i++){
    li(i) = data(i, p-1) - betas(0) - betas(1)*data(i, 0);
  }
  NumericVector li_x(n);
  for(int i = 0; i < n; i++){
    li_x(i) = data(i, 0)*li(i);
  }
  g_out(0) = -2*sum(li)/n;
  g_out(1) = -2*sum(li_x)/n;
  
  return g_out;
}

// [[Rcpp::export]]
double norm(NumericVector g){
  int m = g.length();
  NumericVector g2(m);
  for(int i = 0; i < m; i++){
    g2(i) = g(i)*g(i);
  }
  return(sqrt(sum(g2)));
}

// [[Rcpp::export]]
List epoch_update(NumericMatrix data, NumericVector betas_in, double alpha, int n_epoch, int minibatch_size){
  // Important: we have to clone the vector, otherwise, the original input vector 
  // will be modified in the R environment because they have the same name.
  NumericVector betas = clone(betas_in);
  int n = data.nrow();
  int p = betas.length();
  NumericVector g(p);
  NumericVector g_unit(p);
  int num_iters;
  if(n % minibatch_size == 0) num_iters = floor(n/minibatch_size);
  else num_iters = floor(n/minibatch_size) + 1;
  // cout << "num_iters = " << num_iters <<"\n";
  // int num_iters = floor(n/minibatch_size) + 1;
  NumericMatrix epoch_values(num_iters, 5);
  
  
  for(int k = 0; k < num_iters; k++){
    
    if((k + 1) != num_iters){ // all but last minibatch
      // cout << "k = " << k <<"\n";
      NumericMatrix data_subset(minibatch_size, data.ncol());
      int i = 0;
      int lwr = k*minibatch_size; 
      for(int i = 0; i < data_subset.nrow(); i++){
        for(int j = 0; j < data_subset.ncol(); j++){
          data_subset(i, j) = data(lwr + i, j); 
        }
      }
      g = gradient(data_subset, betas);
    } else {
      // cout << "Último minibatch \n";
      int lwr = k*minibatch_size; 
      int size = data.nrow() - lwr;
      if(size == 0) break;
      // cout << "size = " << size <<"\n";
      NumericMatrix data_subset(size, data.ncol());
      for(int i = 0; i < data_subset.nrow(); i++){
        for(int j = 0; j < data_subset.ncol(); j++){
          data_subset(i, j) = data(lwr + i, j); 
        }
      }
      g = gradient(data_subset, betas);
    }
    
    
    for(int j = 0; j < p; j++){
      betas(j) = betas(j) - alpha*g(j);
    }
    
    //  Print and write values in table to keep track and make plots
    double g_norm = norm(g);
    for(int j = 0; j < p; j++){
      g_unit(j) = g(j)/g_norm;
    }
    epoch_values(k, 0) = n_epoch;
    epoch_values(k, 1) = k + 1;
    epoch_values(k, 2) = betas(0);
    epoch_values(k, 3) = betas(1);
    epoch_values(k, 4) = g_norm;
  } // end for
  
  List ret;
  ret["epoch_values"] = epoch_values;
  ret["betas"] = betas;
  return ret;
}

