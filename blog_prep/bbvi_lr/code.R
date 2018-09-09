library(MASS)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

theme_set(theme_bw())


# Functions ---------------------------------------------------------------

norm = function(x){
  return(sqrt(sum(x^2)))
}

sigmoid = function(w){
  return(1/(1 + exp(-w)))
}

dot = function(X, Y){
  return(X %*% Y)
}

zeros = function(P){
  return(rep(0.0, P))
}

ones = function(P){
  return(rep(1.0, P))
}

norm_logpdf = function(x, mean, sd){
  return(log(dnorm(x, mean, sd)))
}


create_data = function(N, P, real_mu, seed = 0){
  set.seed(seed)
  X = mvrnorm(N, mu = rep(0, P), Sigma = diag(rep(1, P)))
  y = rbinom(N, 1, sigmoid(dot(X, real_mu)))
  return(list(X = X, y = y))
}


elbo_grad = function(z_sample, mu, sigma_sq, y, X, P, prior_sigma){
  score_mu = (z_sample - mu)/(sigma_sq)
  score_logsigma_sq = (-1/(2*sigma_sq) + ((z_sample - mu)^2)/(2*(sigma_sq^2))) * sigma_sq
  aux_1 = y * as.numeric(log(sigmoid(dot(X, z_sample))))
  aux_2 = (1 - y) * as.numeric(log(1 - sigmoid(dot(X, z_sample))))
  log_p_aux_1 = sum(aux_1 + aux_2)
  log_p_aux_2 = sum(log(dnorm(z_sample, zeros(P), prior_sigma*ones(P))))
  log_p = log_p_aux_1 + log_p_aux_2
  log_q = sum(norm_logpdf(z_sample, mu, sqrt(sigma_sq)))
  grad = c(score_mu, score_logsigma_sq)*(log_p - log_q)
  return(grad)
}

vi = function(X, y, prior_sigma = 10000, max_iter = 6000, S = 10, eta = 1.0){
  N = dim(X)[1]
  P = dim(X)[2]
  mu = rnorm(P)
  G = matrix(zeros(2*P*2*P), ncol = 2*P)
  log_sigma_sq = rnorm(P)
  mus = matrix(zeros(max_iter*P), ncol = P)
  delta_lambda = zeros(max_iter)
  
  cat("Begin optimization\n\n")
  for(t in 1:max_iter){
    mus[t,] = mu
    sigma_sq = exp(log_sigma_sq)
    samples = mvrnorm(S, mu, diag(sigma_sq))
    grad_acc = rep(0.0, 2*P)
    for(i in 1:nrow(samples)){
      z_sample = samples[i, ]
      grad_acc = grad_acc + elbo_grad(z_sample, mu, sigma_sq, y, X, P, prior_sigma)
    }
    grad_estimate = grad_acc/S
    G = G + (grad_estimate %*% t(grad_estimate))
    rho_t = (eta * 1/sqrt(diag(G)))
    mu_new = mu + rho_t[1:P] * grad_estimate[1:P]
    log_sigma_sq_new = log_sigma_sq + rho_t[(P+1):(2*P)] * grad_estimate[(P+1):(2*P)]
    delta_lambda_now = norm(mu_new - mu)
    delta_lambda[t] = delta_lambda_now
    if(t %% 100 == 1){
      cat("", "\n")
      cat("Iteration: ", t, "\n")
      cat("Mu: ", mu, "\n")
      cat("Sigma squared: ", exp(log_sigma_sq), "\n")
      cat("Delta lambda: ", delta_lambda_now, "\n")
    }
      
    if(delta_lambda_now < 0.0001){
      cat("Breaking\n\n")
      break
    }
    mu = mu_new
    log_sigma_sq = log_sigma_sq_new
  }
  
  cat(""    , "\n")
  cat("Optimization complete", "\n")
  cat("Final sigma_sq: ", exp(log_sigma_sq), "\n")
  cat("Final mu: ", mu, "\n")
  model_out = list(
    mu = mu, 
    sigma_sq = sigma_sq, 
    mus = mus[1:t,], 
    delta_lambda = delta_lambda[1:t]
  )
  return(model_out)
}


# Run VI ------------------------------------------------------------------

real_mu = c(-2, -1, 1, 2)

dat = create_data(2000, 4, real_mu)

mod = vi(dat$X, dat$y, S = 5, max_iter = 20000)

mu = mod$mu
sigma = sqrt(mod$sigma_sq)
colours = c('red', 'blue', 'dark green', 'green')

tibble(x = c(mu + 3*sigma, mu - 3*sigma)) %>% 
  ggplot(aes(x)) + 
  geom_vline(xintercept = real_mu, col = colours, linetype = "dashed") +
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = col)
  }, 
  mean = mu, 
  sd = sigma, 
  col = colours
  ) 


mod$mus %>% 
  as_tibble() %>% 
  mutate(Iteration = 1:nrow(.)) %>% 
  gather(key, value, -Iteration) %>% 
  ggplot() +
  #geom_hline(yintercept = real_mu, color = "dark grey") +
  geom_line(aes(Iteration, value, colour = key)) +
  ylab("Variational mean") 


tibble(delta_lambda = mod$delta_lambda) %>% 
  mutate(Iteration = 1:nrow(.)) %>% 
  ggplot() +
  geom_line(aes(Iteration, delta_lambda))





