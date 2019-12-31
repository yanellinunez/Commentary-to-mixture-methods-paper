library(tidyverse)
library(janitor)
library(glmnet)
library(openssl)

# Load Data
study_pop = read_csv("Data/studypop.csv") %>% 
  clean_names(case = c("old_janitor")) %>% 
  mutate(bmi_cat3 = as.factor(bmi_cat3),
         edu_cat = as.factor(edu_cat),
         race_cat = as.factor(race_cat),
         male = as.factor(male)) 

data = study_pop %>% 
  mutate_at(vars(contains("la")), log) %>% 
  mutate(log_telomean = log(telomean)) %>% 
  dplyr::select(log_telomean, lbx074la:lbx187la, lbxd03la:lbx194la, everything(), -seqn, -telomean) %>% 
  na.omit(log_telomean) 

# Create a matrix of predictors as x
x = model.matrix(log_telomean ~ ., data)[,-1]

# Extract outcome vector
y = data$log_telomean

# Lasso 

## Looping seeds for lasso for coefficients plot
fit_cv_lasso = function(seed) {
  set.seed(seed)
  cv.lasso = cv.glmnet(x, y, 
                       penalty.factor = c(rep(1, ncol(x[,1:18])), rep(0, ncol(x[,19:36]))),
                       type.measure = "mse", alpha = 1)
  
  # Find the lambda that results in the smallest CV error
  best_lambda <- cv.lasso$lambda.min
  
  # Fit model with cross-validated lambda
  lasso.mod = glmnet(x, y, 
                     penalty.factor = c(rep(1, ncol(x[,1:18])), rep(0, ncol(x[,19:36]))),
                     alpha = 1, lambda = best_lambda)
  lasso.mod
}

extract_coefs = function(fit) {
  as.tibble(as.matrix(fit$beta), rownames = "variable") %>%
    rename(beta = s0) 
}

# Good seeds!
rand_seeds <- rand_num(100)*2^31

repeat_cv_lasso = 
  tibble(seed = as.integer(rand_seeds)) %>% 
  mutate(
    fits = map(seed, ~fit_cv_lasso(seed = .x)),
    coefs = map(fits, ~extract_coefs(fit = .x))) %>% 
  dplyr::select(-fits) %>% 
  unnest() %>%
  rename(beta_lasso = "beta") 

# Lasso visualization
## betas
repeat_cv_lasso  %>%
  ggplot() + 
  geom_point(aes(x = seed, y = beta_lasso), color = 'red', size = 0.5, alpha = 0.5) + 
  facet_wrap(~variable) + 
  geom_hline(yintercept = 0) +
  ggtitle("R3.6.1 glmnet 2.0-18 betas")
