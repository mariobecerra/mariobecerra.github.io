library(reticulate)
library(tidyverse)

theme_set(theme_bw())

# py_run_file("code.py")

source_python("code.py")

real_mu = c(-2, -1, 1, 2)

dat = create_data(200, 4, real_mu)

mod = vi(dat[[1]], dat[[2]]) %>% 
  set_names("mu", "sigma_sq", "mus", "delta_lambda")

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





