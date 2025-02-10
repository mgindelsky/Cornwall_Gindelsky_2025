pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast)

# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = 'vars.R', replacement = '')

data <- read_excel(paste0(root,'/replication_data.xlsx'))

data <- data %>%
  select(year, gini, contains('is_q')) 

gini_predict <- list()
is_q_predict <- list()
for (yr in 2019:2022) {
  temp_data <- data %>%
    filter(year <= yr) %>%
    select(-year)
  #Gini AR2
  gini_mod <- forecast::Arima(temp_data$gini, order = c(2, 0, 0))
  #Quintiles VAR2
  is_q_mod <- vars::VAR(log(temp_data[,paste0('is_q',1:5)]), p = 2)
  gini_predict[[yr - 2018]] <- list(
    model = paste0('Data Ending ', year),
    fitted = gini_mod$fitted,
    next_step = forecast(gini_mod, h = 1)$mean
  )
  q_predict <- predict(is_q_mod, n.ahead = 1)$fcst %>% 
    do.call(rbind,.) %>%
    as_tibble() %>% 
    select(fcst)
  
  is_q_predict[[yr - 2018]] <- list(
    model = paste0('Data Ending ', year),
    fitted = exp(fitted.values(is_q_mod))/rowSums(exp(fitted.values(is_q_mod)))*100,
    next_step = exp(q_predict)/sum(exp(q_predict))*100
  )
}
# Save list objects with results 
save(gini_predict, 
     is_q_predict,
     file = paste0(root, '/var_results.Rda'))
