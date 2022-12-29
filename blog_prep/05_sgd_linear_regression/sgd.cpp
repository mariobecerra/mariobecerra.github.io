#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <iostream>
using namespace std;
using namespace Rcpp;
using namespace RcppArmadillo;

// [[Rcpp::depends(RcppArmadillo)]]


//////////////////////////
/////// FUNCTION 1
//////////////////////////
// [[Rcpp::export]]
double norm(NumericVector g){
  // Receives a numeric vector and returns L2 norm
  int m = g.length();
  NumericVector g2(m);
  for(int i = 0; i < m; i++){
    g2(i) = g(i)*g(i);
  }
  return(sqrt(sum(g2)));
}






//////////////////////////
/////// FUNCTION 2
//////////////////////////
// [[Rcpp::export]]
NumericVector gradient(NumericMatrix data, NumericVector betas){
  // Receives a data matrix and a vector of parameters.
  // Returns the gradient of quadratic loss function for linear regression
  // for the current value of the parameters (betas).
  int n = data.nrow(); // number of data points
  int p = data.ncol(); // number of coefficients (including intercept)
  NumericVector li(n); // vector that will contain each observation's loss
  NumericVector g_out(p); // output gradient vector
  for(int i = 0; i < n; i++){ // iterate over rows
    double sum_betas_xi  = 0.0; // accumulator
    for(int j = 1; j < p; j++){ // iterate over columns
      sum_betas_xi = sum_betas_xi + betas(j)*data(i, j-1);
    }
    li(i) = data(i, p-1) - betas(0) - sum_betas_xi;
  }
  NumericMatrix li_x(n, p-1);
  for(int i = 0; i < n; i++){ // iterate over rows
    for(int j = 0; j < p - 1; j++){ // iterate over columns
      li_x(i, j) = data(i, j)*li(i);
    }
  }
  
  // gradient for intercept
  g_out(0) = -2*sum(li)/n;
  
  // gradient for the rest of the parameters
  for(int j = 0; j < p-1; j++){
    double sum_li_x = 0.0;
    for(int i = 0; i < n; i++){
      sum_li_x = sum_li_x + li_x(i, j);
    }
    g_out(j+1) = -2*sum_li_x/n;
  }
  return g_out;
}






//////////////////////////
/////// FUNCTION 3
//////////////////////////
// [[Rcpp::export]]
List epoch_update(NumericMatrix data, NumericVector betas_in, double alpha, int n_epoch, int minibatch_size){
  // Updates the value of the parameters based on the gradients of minibatch iteration
  // Receives a data matrix, vector of parameters, learning rate alpha, 
  // what epoch is currently running, and minibatch size.
  // Important: we have to clone the vector betas_in, otherwise, the original input vector 
  // will be modified in the R environment because they have the same name (betas).
  NumericVector betas = clone(betas_in); // output vector
  int n = data.nrow(); // number of rows in data matrix
  int p = betas.length(); // number of parameters (including intercept)
  NumericVector g(p); // gradient vector
  int num_iters; // number of iterations (based on minibatch size and data size)
  if(n % minibatch_size == 0) num_iters = floor(n/minibatch_size);
  else num_iters = floor(n/minibatch_size) + 1;
  NumericMatrix epoch_values(num_iters, p+3); // values of parameters in this epoch
  
  // iterate over minibatches
  for(int k = 0; k < num_iters; k++){
    if((k + 1) != num_iters){ // all but last minibatch
      NumericMatrix data_subset(minibatch_size, data.ncol()); // subsets the minibatch
      int i = 0;
      int lwr = k*minibatch_size; // lower index for minibatch
      for(int i = 0; i < data_subset.nrow(); i++){ // subset the data
        for(int j = 0; j < data_subset.ncol(); j++){
          data_subset(i, j) = data(lwr + i, j); 
        }
      }
      g = gradient(data_subset, betas); // compute gradient vector
    } else { // last minibatch
      int lwr = k*minibatch_size; 
      int size = data.nrow() - lwr; // size of last minibatch
      if(size == 0) break; // if last minibatch is of size 0, break
      NumericMatrix data_subset(size, data.ncol());
      for(int i = 0; i < data_subset.nrow(); i++){ // subset data
        for(int j = 0; j < data_subset.ncol(); j++){
          data_subset(i, j) = data(lwr + i, j); 
        }
      }
      g = gradient(data_subset, betas); // compute gradient vector
    }
    
    // Update parameter vector
    for(int j = 0; j < p; j++){
      betas(j) = betas(j) - alpha*g(j);
    }
    
    double g_norm = norm(g); // Gradient norm
    
    // Fill matrix with parameter and norm values
    epoch_values(k, 0) = n_epoch;
    epoch_values(k, 1) = k + 1;
    epoch_values(k, 2) = g_norm;
    for(int j = 0; j < p; j++){
      epoch_values(k, j+3) = betas(j);
    }
  } // end for
  
  // output list of values
  List ret;
  ret["epoch_values"] = epoch_values;
  ret["betas"] = betas;
  return ret;
}

