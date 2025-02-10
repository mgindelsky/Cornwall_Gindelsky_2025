# Replication Script for Personal Income Nowcasting - This script creates the dataframes from which the tables and figures can be created

  # Inputs - Excel file in the directory "replication"
  # Outputs - 
      # results - A list of outputs for the nowcasting of the Gini Coefficient
        # Includes features, coefficients, and predictions (one-step-ahead)
      # resultsq - A list of outputs for the nowcasting of the Quintile Income Shares
        # Includes features, coefficients, and predictions (one-step-ahead) 

# Parameter choices and their values:
  # Sequency of alpha values to check for the elastic net
    alpha_check <- seq(0,1,.01) 
  # Sequence of lambda values to check
    lambda <- seq(.005, 0.50, .005) 
  # A vector of features that will not be penalized
    no_penalty_gini <- c('taxcred', 'wages', 'prop', 'assets')
    no_penalty_q <- c('taxcred', 'wages', 'prop', 'assets')
  # The targeted variance explained (deviance ratio) for the model, will use the value closest to this
    var_val <- .95
    
# Load Packages ####
  require(pacman)
  pacman::p_load(readxl, tidyverse, ggplot2, glmnet, rstudioapi, pracma)

# Directories ####
  root <- getSourceEditorContext()$path %>%    
gsub(pattern = 'results.R', replacement = '')

# Source Helper Function(s) ####
  source(paste0(root, 'plot_results_function.R'))
         
# Load Data ####
  data <- read_excel(paste0(root,'replication_data.xlsx'))
  gini_x <- data %>%
         select(year, assets, medicaid, medicare, prop, rent, taxcred, transfers,	unemployment,	wages)
  targets <- data %>%
         select(year, gini,	is_q1,	is_q2,	is_q3,	is_q4,	is_q5)
  
  q_x <- data %>%
    mutate(l1_q1 = lag(is_q1, 1),
           l1_q2 = lag(is_q2, 1),
           l1_q3 = lag(is_q3, 1),
           l1_q4 = lag(is_q4, 1),
           l1_q5 = lag(is_q5, 1),
           l2_q1 = lag(is_q1, 2),
           l2_q2 = lag(is_q2, 2),
           l2_q3 = lag(is_q3, 2),
           l2_q4 = lag(is_q4, 2),
           l2_q5 = lag(is_q5, 2)) %>%
    select(-contains('is_q'))
    
         
# Gini ####
  # Parameters ####
  penalty <- abs((colnames(gini_x) %in% no_penalty_gini - 1))
  # Storage ####
  results <- list()
  RMSE <- numeric()
  # Processing ####
  # Iteratively do one-step ahead nowcasts for 2020, 2021, 2022, and 2023 ###
  for (i in 1:4) {  
    temp <- gini_x %>%
      makeX(., na.impute = T) #This will impute earlier values if they are NA due to differencing.
    for (j in seq_along(alpha_check)) {
      check <- glmnet(
        family = 'gaussian',
        relax = FALSE,
        lower.limits = -Inf,
        upper.limits = Inf,
        penalty.factor = penalty,
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
    #Final Model based on RMSE minimizing Alpha
    check <- glmnet(
      family = 'gaussian',
      relax = FALSE,
      alpha = alpha_check[which.min(RMSE)],
      lower.limits = -Inf,
      upper.limits = Inf,
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
#Now use the predicted Gini to help nowcast the quintiles

  
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

  # Processing ####
  for (i in 1:4) {
    tempq <- q_x %>%
      bind_cols(.,gini_pred = c(targets$gini[1:(19+i)],
                                results[[i]]$predictions[(20+i):24])) %>%
      select(-contains('unemployment')) %>%
      makeX(., na.impute = T)
    penalty <- abs((colnames(tempq) %in%  no_penalty_q - 1))
    RMSE <- numeric()
    for (j in seq_along(alpha_check)) {
      temp_target <- targets2[[i]]$demeaned
      check <- glmnet(
        family = 'mgaussian',
        standardize = T,
        lambda = lambda,
        penalty.factor = penalty,
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
      lower.limits = -Inf,
      upper.limits = Inf,
      lambda = lambda,
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
  
  # Preparing for Plotting ####
  q1_results <- lapply(resultsq, function(x) {
    data.frame(predictions = x$predictions[,1])})
  
  q2_results <- lapply(resultsq, function(x) {
    data.frame(predictions = x$predictions[,2])})
  
  q3_results <- lapply(resultsq, function(x) {
    data.frame(predictions = x$predictions[,3])})
  
  q4_results <- lapply(resultsq, function(x) {
    data.frame(predictions = x$predictions[,4])})
  
  q5_results <- lapply(resultsq, function(x) {
    data.frame(predictions = x$predictions[,5])})
  
  
  

  
