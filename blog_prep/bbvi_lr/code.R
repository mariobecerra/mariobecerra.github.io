library(MASS)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

theme_set(theme_bw())

max_iter = 20000
eps = 1e-16

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

create_data = function(N, real_mu, seed = 0){
  set.seed(seed)
  P = length(real_mu)
  X = mvrnorm(N, mu = rep(0, P), Sigma = diag(rep(1, P)))
  y = rbinom(N, 1, sigmoid(dot(X, real_mu)))
  return(list(X = X, y = y))
}


# elbo_grad = function(z_sample, mu, sigma_sq, y, X, P, prior_sigma){
#   score_mu = (z_sample - mu)/(sigma_sq)
#   score_logsigma_sq = (-1/(2*sigma_sq) + ((z_sample - mu)^2)/(2*(sigma_sq^2))) * sigma_sq
#   aux_1 = y * as.numeric(log(sigmoid(dot(X, z_sample))))
#   aux_2 = (1 - y) * as.numeric(log(1 - sigmoid(dot(X, z_sample))))
#   log_p_aux_1 = sum(aux_1 + aux_2)
#   log_p_aux_2 = sum(log(dnorm(z_sample, zeros(P), prior_sigma*ones(P))))
#   log_p = log_p_aux_1 + log_p_aux_2
#   log_q = sum(norm_logpdf(z_sample, mu, sqrt(sigma_sq)))
#   grad = c(score_mu, score_logsigma_sq)*(log_p - log_q)
#   return(grad)
# }


# matrix_vector_operation = function(X, y, func){
#   if(func == "+"){
#     out = t(t(X) + y)
#   }
#   if(func == "-"){
#     out = t(t(X) - y)
#   }
#   if(func == "*"){
#     out = t(t(X) * y)
#   }
#   if(func == "/"){
#     out = t(t(X) / y)
#   }
#   return(out)
# }
# 
# 
# matrix_vector_operation_2 = function(X, y, func){
#   if(func == "+"){
#     out = t(t(X) + y)
#   }
#   if(func == "-"){
#     out = t(t(X) - y)
#   }
#   if(func == "*"){
#     out = t(t(X) * y)
#   }
#   if(func == "/"){
#     out = t(t(X) / y)
#   }
#   return(out)
# }
# 
# elbo_grad_samples = function(samples, mu, sigma_sq, y, X, P, prior_sigma){
#   centered = matrix_vector_operation(samples, mu, "-")
#   score_mu = matrix_vector_operation(centered, sigma_sq, "/")
#   score_logsigma_sq_1 = matrix_vector_operation(centered^2, 2*(sigma_sq^2), "/")
#   score_logsigma_sq_2 = matrix_vector_operation(score_logsigma_sq_1, -1/(2*sigma_sq), "+")
#   score_logsigma_sq = matrix_vector_operation(score_logsigma_sq_2, sigma_sq, "*")
#   # score_logsigma_sq = (-1/(2*sigma_sq) + ((z_sample - mu)^2)/(2*(sigma_sq^2))) * sigma_sq
#   sigmoid_matrix = sigmoid(dot(X, t(samples)))
#   aux_1 = matrix_vector_operation((log(sigmoid_matrix)), y, "*")
#   aux_2 = matrix_vector_operation(log(1 - sigmoid_matrix), (1 - y), "*")
#   log_p_aux_1 = colSums(aux_1 + aux_2)
#   log_p_aux_2 = sum(log(dnorm(samples, zeros(P), prior_sigma*ones(P))))
#   log_p = log_p_aux_1 + log_p_aux_2
#   log_q = sum(norm_logpdf(samples, mu, sqrt(sigma_sq)))
#   grad = c(score_mu, score_logsigma_sq)*(log_p - log_q)
#   return(grad)
# }


elbo_grad_samples = function(samples, mu, sigma_sq, y, X, P, prior_sigma){
  grad_acc = rep(0.0, 2*P)
  elbo_acc = 0.0
  S = nrow(samples)
  for(i in 1:S){
    z_sample = samples[i, ]
    centered = z_sample - mu
    score_mu = centered/sigma_sq
    score_logsigma_sq = (-1/(2*sigma_sq) + (centered^2)/(2*(sigma_sq^2))) * sigma_sq
    aux_1 = y * as.numeric(log(sigmoid(dot(X, z_sample)) + eps))
    aux_2 = (1 - y) * as.numeric(log(1 - sigmoid(dot(X, z_sample)) + eps))
    log_p_aux_1 = sum(aux_1 + aux_2)
    log_p_aux_2 = sum(log(dnorm(z_sample, zeros(P), prior_sigma*ones(P))))
    log_p = log_p_aux_1 + log_p_aux_2
    log_q = sum(log(dnorm(z_sample, mu, sqrt(sigma_sq))))
    elbo_i = log_p - log_q
    grad_i = c(score_mu, score_logsigma_sq)*(elbo_i)
    grad_acc = grad_acc + grad_i/S
    elbo_acc = elbo_acc + elbo_i/S
  }
  return(list(grad = grad_acc,
              elbo = elbo_acc))
}


vi = function(X, y, prior_sigma = 10000, max_iter = 6000, S = 10, eta = 1.0, seed = 0){
  set.seed(seed)
  N = dim(X)[1]
  P = dim(X)[2]
  mu = rnorm(P)
  G = matrix(zeros(2*P*2*P), ncol = 2*P)
  log_sigma_sq = rnorm(P)
  mus = matrix(zeros(max_iter*P), ncol = P)
  norm_delta_lambda = zeros(max_iter)
  ELBO = zeros(max_iter)
  
  cat("Begin optimization\n\n")
  for(t in 1:max_iter){
    mus[t,] = mu
    sigma_sq = exp(log_sigma_sq)
    samples = mvrnorm(S, mu, diag(sigma_sq))
    elbo_grad = elbo_grad_samples(samples, mu, sigma_sq, y, X, P, prior_sigma)
    grad_estimate = elbo_grad$grad
    ELBO[t] = elbo_grad$elbo
    G = G + (grad_estimate %*% t(grad_estimate))
    rho_t = (eta * 1/sqrt(diag(G)))
    mu_new = mu + rho_t[1:P] * grad_estimate[1:P]
    log_sigma_sq_new = log_sigma_sq + rho_t[(P+1):(2*P)] * grad_estimate[(P+1):(2*P)]
    norm_delta_lambda_t = norm(mu_new - mu)
    norm_delta_lambda[t] = norm_delta_lambda_t
    stop_criterion = norm_delta_lambda_t/norm(mu)
    if(t %% 100 == 1){
      cat("", "\n")
      cat("Iteration: ", t, "\n")
      cat("Mu: ", mu, "\n")
      cat("Sigma squared: ", exp(log_sigma_sq), "\n")
      cat("Delta lambda: ", norm_delta_lambda_t, "\n")
      cat("Estimate gradient norm: ", norm(grad_estimate), "\n")
      cat("Estimate ELBO: ", ELBO[t], "\n")
      cat("Stop criterion value: ", stop_criterion, "\n")
    }
    if(stop_criterion < 0.00001){
      cat("Breaking\n\n")
      break
    }
    mu = mu_new
    log_sigma_sq = log_sigma_sq_new
  }
  
  cat(""    , "\n")
  cat("Optimization complete", "\n")
  cat("Final mu: ", mu, "\n")
  cat("Final sigma_sq: ", exp(log_sigma_sq), "\n")
  cat("Stop criterion value: ", stop_criterion, "\n")
  model_out = list(
    mu = mu, 
    sigma_sq = sigma_sq, 
    mus = mus[1:t,], 
    norm_delta_lambda = norm_delta_lambda[1:t],
    ELBO = ELBO[1:t]
  )
  return(model_out)
}


# Run VI ------------------------------------------------------------------

real_mu = c(-10, 10)

dat = create_data(300, real_mu, seed = 0)

mod = vi(dat$X, dat$y, S = 30, max_iter = max_iter, seed = 0)


# VI results --------------------------------------------------------------

mu = mod$mu
sigma = sqrt(mod$sigma_sq)
colours = c('red', 'blue', 'dark green', 'green')[1:length(real_mu)]

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
  geom_hline(yintercept = real_mu, linetype = "dashed") +
  geom_line(aes(Iteration, value, colour = key)) +
  ylab("Variational mean") 

mod$mus %>% 
  as_tibble() %>% 
  ggplot() +
  geom_point(data = tibble(V1 = real_mu[1], V2 = real_mu[2]),
             aes(V1, V2),
             colour = "black", fill = "white", size = 2, stroke = 2, shape = 21) +
  geom_path(aes(V1, V2), size = 0.3, alpha = 0.7) 

tibble(ELBO = mod$ELBO) %>% 
  mutate(Iteration = 1:nrow(.)) %>% 
  ggplot() +
  geom_line(aes(Iteration, ELBO))


tibble(norm_delta_lambda = mod$norm_delta_lambda) %>% 
  mutate(Iteration = 1:nrow(.)) %>% 
  ggplot() +
  geom_line(aes(Iteration, norm_delta_lambda))




# Posterior predictive ----------------------------------------------------

test_dat = create_data(20, real_mu, seed = 0)

post_samples = mvrnorm(1000, mu = mod$mu, Sigma = diag(mod$sigma_sq))

probs = sigmoid(post_samples %*% t(test_dat$X))

mean_probs = colMeans(probs)

preds = ifelse(mean_probs > 0.5, 1, 0)

table(preds, test_dat$y)
