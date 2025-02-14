pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast)
#Replication for Tables


# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = 'table_working.R', replacement = '')

load(file = paste0(root, '/var_predict_results.Rda'))
load(file = paste0(root, '/enet_predict_results.Rda'))

# Replication for Top Panel ####
rmse_imp <- function(observed, var, enet) {
  rmse_var <- sqrt(mean((observed - var)^2))
  rmse_enet <- sqrt(mean((observed - enet)^2))
  imp <- 1- rmse_enet/rmse_var
  return(imp)
}

gini_rmse_imp <- q1_rmse_imp <- q2_rmse_imp <-  numeric()
q3_rmse_imp <- q4_rmse_imp <- q5_rmse_imp <- numeric()

# Calculate RMSE improvement ####
for (i in 1:4) {
  g_temp <- data.frame(
    true = gini_dat$true,
    enet = gini_dat[,i + 2],
    var = vgini_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  gini_rmse_imp[i] <- rmse_imp(
    observed = g_temp$true,
    var = g_temp$var,
    enet = g_temp$enet)
  
  q1_temp <- data.frame(
    true = q1_dat$true,
    enet = q1_dat[,i + 2],
    var = vq1_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  q1_rmse_imp[i] <- rmse_imp(
    observed = q1_temp$true,
    var = q1_temp$var,
    enet = q1_temp$enet)
  
  q2_temp <- data.frame(
    true = q2_dat$true,
    enet = q2_dat[,i + 2],
    var = vq2_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  q2_rmse_imp[i] <- rmse_imp(
    observed = q2_temp$true,
    var = q2_temp$var,
    enet = q2_temp$enet)
  
  q3_temp <- data.frame(
    true = q3_dat$true,
    enet = q3_dat[,i + 2],
    var = vq3_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  q3_rmse_imp[i] <- rmse_imp(
    observed = q3_temp$true,
    var = q3_temp$var,
    enet = q3_temp$enet)
  
  q4_temp <- data.frame(
    true = q4_dat$true,
    enet = q4_dat[,i + 2],
    var = vq4_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  q4_rmse_imp[i] <- rmse_imp(
    observed = q4_temp$true,
    var = q4_temp$var,
    enet = q4_temp$enet)
  
  q5_temp <- data.frame(
    true = q5_dat$true,
    enet = q5_dat[,i + 2],
    var = vq5_dat[,i + 2]) %>%
    filter(complete.cases(.))
  
  q5_rmse_imp[i] <- rmse_imp(
    observed = q5_temp$true,
    var = q5_temp$var,
    enet = q5_temp$enet)
  
}
# Form RMSE Improvement Table ####
rmse_improvement <- data.frame(
  Q1 = q1_rmse_imp,
  Q2 = q2_rmse_imp,
  Q3 = q3_rmse_imp,
  Q4 = q4_rmse_imp,
  Q5 = q5_rmse_imp,
  Gini = gini_rmse_imp) %>%
  mutate(across(everything(),
                ~round(.x*100, 1))) %>%
  mutate(Data_End = 2019:2022) %>%
  relocate(Data_End)

# Turning Point Analysis ####
gini_tp <- gini_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

q1_tp <- q1_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

q2_tp <- q2_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

q3_tp <- q3_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

q4_tp <- q4_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

q5_tp <- q5_dat %>%
  filter(year > 2000) %>%
  mutate(across(-year, 
                .fns = ~.x - lag(.x))) %>%
  mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_) %>%
  mutate(across(everything(), ~round(.x*100)))

# Form Turning Point Table ####
tp_table <- gini_tp %>%
  bind_rows(q1_tp, 
            q2_tp, 
            q3_tp, 
            q4_tp, 
            q5_tp) 
# Print Tables ####
rmse_improvement
tp_table
