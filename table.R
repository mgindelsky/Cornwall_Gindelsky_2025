rm(list=ls())
pacman::p_unload(all)
pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast, hablar)
#Replication for Tables
  # Note that the code below replicates the tables from the paper using 
  # publicly available data. This data is reported at three digit precision. 
  # As a result, these tables will not match the tables in the paper exactly.


# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = 'table.R', replacement = '')

data <- read_excel(paste0(root,'/replication_data.xlsx'))
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
  mutate(Year = paste(2019:2022)) %>%
  relocate(Year)

# Covid only RMSE Improvemnt ####
covid_only <- data.frame(
  Year = 'Covid (2020:2022)',
  Q1 = rmse_imp(observed = q1_dat$true[21:23] %>% as.numeric(.), 
                var = vq1_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                enet = q1_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.)),
  Q2 = rmse_imp(observed = q2_dat$true[21:23] %>% as.numeric(.),
                var = vq2_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                enet = q2_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.)),
  Q3 = rmse_imp(observed = q3_dat$true[21:23] %>% as.numeric(.),
                var = vq3_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                enet = q3_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.)),
  Q4 = rmse_imp(observed = q4_dat$true[21:23] %>% as.numeric(.),
                var = vq4_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                enet = q4_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.)),
  Q5 = rmse_imp(observed = q5_dat$true[21:23] %>% as.numeric(.),
                var = vq5_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                enet = q5_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.)),
  Gini = rmse_imp(observed = gini_dat$true[21:23] %>% as.numeric(.),
                  var = vgini_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.),
                  enet = gini_dat %>% fill(everything()) %>% filter(year == 2023) %>% dplyr::select(-year, -predictions_e22, -true) %>% as.numeric(.))) %>%
  mutate(across(-Year,
                ~round(.x*100, 1)))

rmse_improvement <- rmse_improvement %>%
  bind_rows(., covid_only) 
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

# Covid only TP ####
gini_tpc <- gini_dat %>%
  filter(year %in% 2019:2022) %>%
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

q1_tpc <- q1_dat %>%
  filter(year %in% 2019:2022) %>%
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

q2_tpc <- q2_dat %>%
  filter(year %in% 2019:2022) %>%
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

q3_tpc <- q3_dat %>%
  filter(year %in% 2019:2022) %>%
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

q4_tpc <- q4_dat %>%
  filter(year %in% 2019:2022) %>%
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

q5_tpc <- q5_dat %>%
  filter(year %in% 2019:2022) %>%
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

covid_only <- data.frame(
  Ending = 'Covid (2020-2022)',
  Q1 = rowMeans(q1_tpc),
  Q2 = rowMeans(q2_tpc),
  Q3 = rowMeans(q3_tpc),
  Q4 = rowMeans(q4_tpc), 
  Q5 = rowMeans(q5_tpc),
  Gini = rowMeans(gini_tpc))

# Form Turning Point Table ####
tp_table <- gini_tp %>%
  bind_rows(q1_tp, 
            q2_tp, 
            q3_tp, 
            q4_tp, 
            q5_tp) %>%
  mutate(Variable = c('Gini', 'Q1', 'Q2', 'Q3', 'Q4', 'Q5')) %>%
  relocate(Variable) %>%
  pivot_longer(cols = -Variable,
               names_to = 'Ending',
               values_to = 'Value') %>%
  mutate(Ending = 2000 +  parse_number(Ending)) %>%
  pivot_wider(names_from = Variable,
              values_from = Value) %>% 
  relocate(Ending, Q1, Q2, Q3, Q4, Q5, Gini) %>%
  mutate(Ending = as.character(Ending)) %>%
  bind_rows(., covid_only)

# Form "Nowcast Revisions" Table ####
# Calculate "revision" for each nowcast as "actual-predicted" ####
predictions <- gini_dat %>%
  mutate(preds = true) %>%
  mutate(preds = case_when(
    year == 2021 ~ predictions_e19[which(year == 2021)],
    year == 2022 ~ predictions_e20[which(year == 2022)],
    year == 2023 ~ predictions_e21[which(year == 2023)],
    year == 2024 ~ predictions_e22[which(year == 2024)],
    TRUE ~ preds)) %>%
  select(year, true, preds) %>%
  mutate(revision = true - preds) %>%
  filter(year > 2020) %>%
  summarise(mean_revision = mean(revision, na.rm = T), mean_absolute_revision = mean(abs(revision), na.rm = T)) %>%
  pivot_longer(cols = everything(),
               names_to = 'metric',
               values_to = 'value') 

rev_table <- data %>%
  select(year, is_q1, is_q2, is_q3, is_q4, is_q5) %>%
  mutate(
    pred_is_q1 = case_when(
    year == 2021 ~ q1_dat$predictions_e19[which(q1_dat$year == 2021)],
    year == 2022 ~ q1_dat$predictions_e20[which(q1_dat$year == 2022)],
    year == 2023 ~ q1_dat$predictions_e21[which(q1_dat$year == 2023)],
    year == 2024 ~ q1_dat$predictions_e22[which(q1_dat$year == 2024)],
    TRUE ~ is_q1), 
    pred_is_q2 = case_when(
    year == 2021 ~ q2_dat$predictions_e19[which(q2_dat$year == 2021)],
    year == 2022 ~ q2_dat$predictions_e20[which(q2_dat$year == 2022)],
    year == 2023 ~ q2_dat$predictions_e21[which(q2_dat$year == 2023)],
    year == 2024 ~ q2_dat$predictions_e22[which(q2_dat$year == 2024)],
    TRUE ~ is_q2),
    pred_is_q3 = case_when(
    year == 2021 ~ q3_dat$predictions_e19[which(q3_dat$year == 2021)],
    year == 2022 ~ q3_dat$predictions_e20[which(q3_dat$year == 2022)],
    year == 2023 ~ q3_dat$predictions_e21[which(q3_dat$year == 2023)],
    year == 2024 ~ q3_dat$predictions_e22[which(q3_dat$year == 2024)],
    TRUE ~ is_q3),
    pred_is_q4 = case_when(
    year == 2021 ~ q4_dat$predictions_e19[which(q4_dat$year == 2021)],
    year == 2022 ~ q4_dat$predictions_e20[which(q4_dat$year == 2022)],
    year == 2023 ~ q4_dat$predictions_e21[which(q4_dat$year == 2023)],
    year == 2024 ~ q4_dat$predictions_e22[which(q4_dat$year == 2024)],
    TRUE ~ is_q4),
    pred_is_q5 = case_when(
    year == 2021 ~ q5_dat$predictions_e20[which(q5_dat$year == 2021)],
    year == 2022 ~ q5_dat$predictions_e21[which(q5_dat$year == 2022)],
    year == 2023 ~ q5_dat$predictions_e22[which(q5_dat$year == 2023)],
    year == 2024 ~ q5_dat$predictions_e23[which(q5_dat$year == 2024)],
    TRUE ~ is_q5)) %>%
  mutate(revision_is_q1 = is_q1 - pred_is_q1,
         revision_is_q2 = is_q2 - pred_is_q2,
         revision_is_q3 = is_q3 - pred_is_q3,
         revision_is_q4 = is_q4 - pred_is_q4,
         revision_is_q5 = is_q5 - pred_is_q5) %>%
  filter(year > 2019) %>%
  summarise(q1_mean_revision = mean(revision_is_q1, na.rm = T), q1_mean_abs_revision = mean(abs(revision_is_q1), na.rm = T),
            q2_mean_revision = mean(revision_is_q2, na.rm = T), q2_mean_abs_revision = mean(abs(revision_is_q2), na.rm = T),
            q3_mean_revision = mean(revision_is_q3, na.rm = T), q3_mean_abs_revision = mean(abs(revision_is_q3), na.rm = T),
            q4_mean_revision = mean(revision_is_q4, na.rm = T), q4_mean_abs_revision = mean(abs(revision_is_q4), na.rm = T),
            q5_mean_revision = mean(revision_is_q5, na.rm = T), q5_mean_abs_revision = mean(abs(revision_is_q5), na.rm = T)) %>%
  pivot_longer(cols = everything(),
               names_to = 'variable',
               values_to = 'value') %>%
  mutate(quintile = substr(variable, 1, 2),
         metric = case_when(
           grepl('abs', variable) == T ~ 'Mean Absolute Revision',
           grepl('abs', variable) == F ~ 'Mean Revision')) %>%
  select(-variable) %>%
  pivot_wider(names_from = quintile,
              values_from = value) %>%
  mutate(across(-metric,
                ~round(.x, 1))) %>%
  mutate(Gini = round(predictions$value,1)) %>%
  as.data.frame()


# Print Tables ####
rmse_improvement
tp_table
rev_table
