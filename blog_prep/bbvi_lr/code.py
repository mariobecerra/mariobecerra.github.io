# http://keyonvafa.com/logistic-regression-bbvi/
# https://github.com/keyonvafa/logistic-reg-bbvi-blog

import numpy as np
from scipy.special import expit as sigmoid
from scipy.stats import norm

def elbo_grad(z_sample, mu, sigma_sq, y, X, P, prior_sigma):
    score_mu = (z_sample - mu)/(sigma_sq)
    score_logsigma_sq = (-1/(2*sigma_sq) + np.power((z_sample - mu),2)/(2*np.power(sigma_sq,2))) * sigma_sq
    log_p = np.sum(y * np.log(sigmoid(np.dot(X,z_sample))) + (1-y) * np.log(1-sigmoid(np.dot(X,z_sample))))\
        + np.sum(norm.logpdf(z_sample, np.zeros(P), prior_sigma*np.ones(P)))
    log_q = np.sum(norm.logpdf(z_sample, mu, np.sqrt(sigma_sq)))
    return np.concatenate([score_mu,score_logsigma_sq])*(log_p - log_q)


def vi(X, y, prior_sigma = 10000, max_iter = 6000, S = 10, eta = 1.0):
  N = X.shape[0]
  P = X.shape[1]
  rs = np.random.RandomState(0)
  mu = rs.randn(P)
  G = np.zeros((2*P,2*P))
  log_sigma_sq = rs.randn(P)
  mus = np.zeros((max_iter,P))
  delta_lambda = np.zeros(max_iter)
  
  print "Beginning to optimize"
  for t in xrange(max_iter):
    mus[t] = mu
    sigma_sq = np.exp(log_sigma_sq)
    samples = np.array([rs.normal(mu, np.sqrt(sigma_sq)) for s in xrange(S)])
    grad_estimate = np.mean(np.array([elbo_grad(z_sample, mu, sigma_sq, y, X, P, prior_sigma) for z_sample in samples]),axis=0)
    # print grad_estimate
    G = G + np.outer(grad_estimate, grad_estimate)
    rho_t = (eta * 1/np.sqrt(np.diag(G)))[:P]
    #print rho_t
    mu_new = mu + rho_t * grad_estimate[:P]
    log_sigma_sq_new = log_sigma_sq + (eta * 1/np.sqrt(np.diag(G)))[P:] * grad_estimate[P:]
    delta_lambda_now = np.linalg.norm(mu_new-mu)
    delta_lambda[t] = delta_lambda_now
    if t % 100 == 0:
      print ""
      print "Iteration: ", t
      print "Mu: ", mu
      print "Sigma squared: ", np.exp(log_sigma_sq)
      print "Delta lambda: ", delta_lambda_now
    if delta_lambda_now < 0.0001:
      print "Breaking"
      break
    mu = mu_new
    log_sigma_sq = log_sigma_sq_new
  
  print ""    
  print "Optimization complete"
  print "Final sigma_sq: ", np.exp(log_sigma_sq)
  print "Final mu: ", mu
  return mu, sigma_sq, mus[0:t,], delta_lambda[0:t]

def create_data(N, P, z_real, seed = 0):
  # N = 200
  # P = 4
  N = int(N)
  P = int(P)
  rs = np.random.RandomState(seed)
  X = rs.randn(N,P)
  # z_real = rs.randn(P)
  y = rs.binomial(1,sigmoid(np.dot(X,z_real)))
  return X, y
  
