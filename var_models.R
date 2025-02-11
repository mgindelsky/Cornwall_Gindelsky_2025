pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast)

# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = '/var_models.R', replacement = '')

data <- read_excel(paste0(root,'/replication_data.xlsx'))

data <- data %>%
  dplyr::select(year, gini, contains('is_q')) 

gini_predict <- list()
is_q_predict <- list()
for (yr in 2019:2022) {
  temp_data <- data %>%
    filter(year <= yr) %>%
    dplyr::select(-year)
  #Gini AR2
  gini_mod <- forecast::Arima(temp_data$gini, order = c(2, 0, 0))
  #Quintiles VAR2
  is_q_mod <- vars::VAR(log(temp_data[,paste0('is_q',1:5)]), p = 2)
  gini_predict[[yr - 2018]] <- list(
    model = paste0('Data Ending ', yr),
    fitted = fitted.values(gini_mod),
    next_step = predict(gini_mod, n.ahead = 1)$pred
  )
  q_predict <- predict(is_q_mod, n.ahead = 1)$fcst %>% 
    do.call(rbind,.) %>%
    as_tibble() %>% 
    dplyr::select(fcst)
  
  is_q_predict[[yr - 2018]] <- list(
    model = paste0('Data Ending ', yr),
    fitted = exp(fitted.values(is_q_mod))/rowSums(exp(fitted.values(is_q_mod)))*100,
    next_step = exp(q_predict)/sum(exp(q_predict))*100
  )
}
# Save list objects with results 
save(gini_predict, 
     is_q_predict,
     file = paste0(root, '/var_results.Rda'))
 
