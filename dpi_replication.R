# Replication Script for Disposable Personal Income Nowcasting
  # Inputs - None, the script is self contained based on replication file
  # Outputs - 
    # results - A list of outputs for the nowcasting of the Gini Coefficient
      # Includes features, coefficients, and predictions (one-step-ahead)
    # resultsq - A list of outputs for the nowcasting of the Quintile Income Shares
      # Includes features, coefficients, and predictions (one-step-ahead) 


# Load Packages ####
require(pacman)
pacman::p_load(readxl, tidyverse, ggplot2, glmnet, rstudioapi, pracma)

# Directories ####
root <- getSourceEditorContext()$path %>%
  gsub(pattern = 'dpi_replication.R', replacement = '')
root <- gsub(pattern = 'Results Scripts/', replacement = '', x = root)
data_dir <- paste0(root, 'Base Data/main_analysis_dpi')

# Source Helper Function(s) ####
source(paste0(root, '/Helper Functions/plot_results_function.R'))
# Load Data ####
load(paste0(data_dir, '/gini_features.Rda')) 
load(paste0(data_dir, '/targets.Rda'))
load(paste0(data_dir, '/quintile_features.Rda'))

  # Gini ####
  # Parameters ####
var_val <- .95
dfmax_use <- 50
relax = F
lambda <- seq(.005, 0.50, .005)
ll <- -Inf
ul <- Inf
alpha_check <- seq(0,1,.01)
penalty <- abs((colnames(gini_x) %in% c('share_tax_credits', 'share_wages', 'share_prop', 'share_assets') - 1))

  # Storage ####
results <- list()
RMSE <- numeric()

  # Processing ####
for (i in 1:4) {  
  temp <- gini_x %>%
    makeX(., na.impute = T)
  for (j in seq_along(alpha_check)) {
    check <- glmnet(
      family = 'gaussian',
      relax = relax,
      lower.limits = ll,
      penalty.factor = penalty,
      upper.limits = ul,
      alpha = alpha_check[j], 
      x = temp[1:(19 + i),],
      y = targets %>%
        filter(year %in% 2000:(2018 + i)) %>%
        select(gini) %>%
        makeX(., na.impute = F)
    )
    s_use <- check$lambda[which.min(abs(var_val-check$dev.ratio))]
    predictions <- predict(check, newx = temp, 
                           s = s_use)
    RMSE[j] <- sum((predictions - targets %>% select(gini))^2)
  }
  check <- glmnet(
    family = 'gaussian',
    relax = relax,
    alpha = alpha_check[which.min(RMSE)],
    lower.limits = ll,
    upper.limits = ul,
    penalty.factor = penalty,
    x = temp[1:(19 + i),],
    y = targets %>%
      filter(year %in% 2000:(2018 + i)) %>%
      select(gini) %>%
      makeX(., na.impute = F)
  )
  s_use <- check$lambda[which.min(abs(var_val-check$dev.ratio))]
  coefs <- coef.glmnet(check, s = s_use)
  predictions <- predict(check, newx = temp, 
                         s = s_use)
  results[[i]] <- list(alpha =  alpha_check[which.min(RMSE)],
                       model = check,
                       coefficients = coefs,
                       predictions = predictions)
}


# Quintiles ####
  # Storage ####
targets2 <- list()
resultsq <- list()
  # Target Processing ####
for (j in 1:4) {
  means <- targets %>%
    filter(year %in% 2000:(2018 + j)) %>%
    select(-year, -gini) %>%
    summarize_all(mean)
  
  demeaned_targets <- targets %>%
    filter(year %in% 2000:(2018 + j)) %>%
    select(-year, -gini) 
  
  for (q in 1:5) {
    demeaned_targets[,q] <- demeaned_targets[,q] - as.vector(means[1,q])
  }
  
  targets2[[j]] <- list(
    mean = means,
    demeaned = demeaned_targets
  )
  
}
  # Parameters ####
var_val <- .95

  # Processing ####
for (i in 1:4) {
  tempq <- q_x %>%
    bind_cols(.,gini_pred = c(targets$gini[1:(19+i)],
                              results[[i]]$predictions[(20+i):24])) %>%
    select(-contains('unemployment')) %>%
    makeX(., na.impute = T)
  penalty <- abs((colnames(tempq) %in% c('share_wages', 'share_prop', 
                                       'share_assets', 'share_tax_credits') - 1))
  
  alpha_check <- seq(0,1,.05)
  RMSE <- numeric()
  for (j in seq_along(alpha_check)) {
    temp_target <- targets2[[i]]$demeaned
    check <- glmnet(
      family = 'mgaussian',
      standardize = T,
      lower.limits = ll,
      upper.limits = ul,
      lambda = lambda,
      penalty.factor = penalty,
      # standardize.response = T,
      # intercept = F,
      alpha = alpha_check[j], 
      x = tempq[1:(19 + i),],
      y = temp_target
    )
    
    s_use <- check$lambda[which.min(abs(var_val-check$dev.ratio))]
    predictions2 <- predict(check, newx = tempq, 
                            s = s_use) %>% 
      matrix(., nrow = 24, ncol = 5)
    
    RMSE[j] <- sum((predictions2[1:(19+i)] - targets %>% 
                      filter(year %in% 2000:(2018 + i)) %>%
                      select(-year, -gini))^2)
    
  }
  
  check <- glmnet(
    family = 'mgaussian',
    standardize = T,
    # intercept = F,
    lower.limits = ll,
    upper.limits = ul,
    lambda = lambda,
    # standardize.response = T,
    penalty.factor = penalty,
    alpha = alpha_check[which.min(RMSE)],
    x = tempq[1:(19 + i),],
    y = temp_target
  )
  
  s_use <- check$lambda[which.min(abs(var_val-check$dev.ratio))]
  coefs <- coef.glmnet(check, s = s_use) %>%
    do.call(cbind,.)
  
  predictions2 <- predict(check, newx = tempq, 
                          s = s_use) %>% 
    matrix(., nrow = 24)
  
  
  trash <- targets2[[i]]
  predictions2 <- apply(predictions2, 
                        1, 
                        function(x) {x + trash$mean}) %>%
    do.call(rbind,.)
  resultsq[[i]] <- list(
    alpha =  alpha_check[which.min(RMSE)],
    model = check,
    coefficients = coefs,
    predictions = predictions2)
}
# Plot Results Example ####
plot_results('Gini Coefficient',targets$gini, results,
             dir = paste0(root,'/Images'))

