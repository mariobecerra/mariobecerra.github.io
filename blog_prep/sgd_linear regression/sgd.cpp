#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace RcppArmadillo;

// [[Rcpp::depends(RcppArmadillo)]]

// // [[Rcpp::export]]
// NumericVector plusTwo(NumericVector x){
//   NumericVector out = x + 2.0;
//   return out;
// }
// 
// // [[Rcpp::export]]
// NumericVector giveOutput(NumericVector a){
//   NumericVector b = plusTwo(a);
//   return b;
// }

// gradient <- function(data, betas){
//   n <- nrow(data)
//   const <- rep(1, nrow(data))
//   li <- data$y - betas[1]*const - betas[2]*data$x
//   g1 <- -2*sum(li)/n
//   g2 <- -2*sum(li*data$x)/n
//   return(c(g1, g2))
// }
// 
// gradient_row <- function(data_row, betas){
//   li <- data_row$y - betas[1] - betas[2]*data_row$x
//   g1 <- -2*li
//   g2 <- -2*li*data_row$x
//   return(c(g1, g2))
// }

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
List epoch_update(NumericMatrix data, NumericVector betas_in, double alpha, int n_epoch){
  // Important: we have to clone the vector, otherwise, the original input vector 
  // will be modified in the R environment because they have the same name.
  NumericVector betas = clone(betas_in);
  int n = data.nrow();
  int p = betas.length();
  NumericMatrix epoch_values(n, 5);
  NumericVector g(p);
  NumericVector g_unit(p);
  
  for(int k = 0; k < n; k++){
    NumericMatrix data_subset(1, data.ncol());
    int i = 0;
    // for(int i = 0; i < data_subset.nrow(); i++){
      for(int j = 0; j < data_subset.ncol(); j++){
        data_subset(i, j) = data(k, j); // para mini-batch esto tiene que cambiar
    //  }
    }
    
    g = gradient(data_subset, betas);
    for(int j = 0; j < p; j++){
      betas(j) = betas(j) - alpha*g(j);
    }
    
    //  Print and write values in table to keep track and make plots
    double g_norm = norm(g);
    for(int j = 0; j < p; j++){
      g_unit(j) = g(j)/g_norm;
    }
    epoch_values(k, 0) = n_epoch + 1;
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

// epoch_update <- function(data, betas, alpha, n_epoch, verbose = 1, reordering = F){
//   n <- nrow(data)
//   if(reordering) data$ix <- sample(1:n)
//   else data$ix <- 1:n
//   epoch_values <- tibble(
//     n_epoch = rep(n_epoch, n),
//     obs = 1:n,
//     beta_0 = rep(0, n),
//     beta_1 = rep(0, n),
//     gradient_norm = rep(0, n))
//   
//   # Iterate over rows in data
//   # Not the best practice in R to use for loops, but it's to illustrate
//   for(i in 1:n){
//     # Update coefficients
//     g <- gradient_row(data[data$ix == i,], betas) 
//     betas <- betas - alpha*g
//     # Print and write values in table to keep track and make plots
//     g_norm <- sqrt(sum(g^2))
//     g_unit <- g/g_norm
//     epoch_values$beta_0[i] <- betas[1]
//     epoch_values$beta_1[i] <- betas[2]
//     epoch_values$gradient_norm[i] <- g_norm
//     if(verbose == 2){
//       cat(
//         "\n\tEpoch: ", n_epoch,
//         "\n\tObs: ", i, 
//         "\n\tbeta_0: ", betas[1], 
//         "\n\tbeta_1: ", betas[2],
//         "\n\tgradient_norm: ", g_norm, 
//         "\n\tDirection: ", g_unit[1], g_unit[2],
//         "\n")
//     }
//   } # End for
//   
//   if(verbose == 1){
//     cat(
//       "\n\tEpoch: ", n_epoch,
//       "\n\tbeta_0: ", epoch_values$beta_0[n],
//       "\n\tbeta_1: ", epoch_values$beta_1[n],
//       "\n\tgradient_norm: ", epoch_values$gradient_norm[n],
//       "\n")
//   }
//   return(list(
//     epoch_values = epoch_values,
//     betas = betas
//   ))
// }
