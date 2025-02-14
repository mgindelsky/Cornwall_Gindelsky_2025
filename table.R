pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast,)
#Replication for Tables


# Directories ####
  root <- getSourceEditorContext()$path %>%    
    gsub(pattern = 'table.R', replacement = '')
  
  load(file = paste0(root, '/var_results.Rda'))
  load(file = paste0(root, '/main_results.Rda'))
  data <- read_excel(paste0(root,'replication_data.xlsx'))
# Replication for Top Panel ####

rmse_imp <- function(observed, var, enet) {
  rmse_var <- sqrt(mean((observed - var)^2))
  rmse_enet <- sqrt(mean((observed - enet)^2))
  imp <- 1- rmse_enet/rmse_var
  return(imp)
}

gini_rmse_imp <- numeric()
q1_rmse_imp <- numeric()
q2_rmse_imp <- numeric()
q3_rmse_imp <- numeric()
q4_rmse_imp <- numeric()
q5_rmse_imp <- numeric()


#Calculate RMSE improvement from fit + forecast
for (i in 1:4) {
  gini_rmse_imp[i] <- rmse_imp(
    observed = data$gini[1:(20+i)],
    var = c(gini_predict[[i]]$fitted, gini_predict[[i]]$next_step),
    enet = results[[i]]$predictions[1:(20+i)])
  
  q1_rmse_imp[i] <- rmse_imp(
    observed = data$is_q1[3:(20+i)],
    var = c(is_q_predict[[i]]$fitted[,1], is_q_predict[[i]]$next_step$fcst[1]),
    enet = q1_results[[i]]$predictions[3:(20+i)])
  
  q2_rmse_imp[i] <- rmse_imp(
    observed = data$is_q2[3:(20+i)],
    var = c(is_q_predict[[i]]$fitted[,2], is_q_predict[[i]]$next_step$fcst[2]),
    enet = q2_results[[i]]$predictions[3:(20+i)])
  
  q3_rmse_imp[i] <- rmse_imp(
    observed = data$is_q3[3:(20+i)],
    var = c(is_q_predict[[i]]$fitted[,3], is_q_predict[[i]]$next_step$fcst[3]),
    enet = q3_results[[i]]$predictions[3:(20+i)])
  
  q4_rmse_imp[i] <- rmse_imp(
    observed = data$is_q4[3:(20+i)],
    var = c(is_q_predict[[i]]$fitted[,4], is_q_predict[[i]]$next_step$fcst[4]),
    enet = q4_results[[i]]$predictions[3:(20+i)])
  
  q5_rmse_imp[i] <- rmse_imp(
    observed = data$is_q5[3:(20+i)],
    var = c(is_q_predict[[i]]$fitted[,5], is_q_predict[[i]]$next_step$fcst[5]),
    enet = q5_results[[i]]$predictions[3:(20+i)])

}

#Calculate RMSE improvement from covid forecasts only

var_covid_gini <- c(gini_predict[[1]]$next_step, gini_predict[[2]]$next_step, 
                gini_predict[[3]]$next_step, gini_predict[[4]]$next_step)

var_covid_q1 <- c(is_q_predict[[1]]$next_step$fcst[1], is_q_predict[[2]]$next_step$fcst[1], 
              is_q_predict[[3]]$next_step$fcst[1], is_q_predict[[4]]$next_step$fcst[1])

var_covid_q2 <- c(is_q_predict[[1]]$next_step$fcst[2], is_q_predict[[2]]$next_step$fcst[2],
              is_q_predict[[3]]$next_step$fcst[2], is_q_predict[[4]]$next_step$fcst[2])

var_covid_q3 <- c(is_q_predict[[1]]$next_step$fcst[3], is_q_predict[[2]]$next_step$fcst[3],
              is_q_predict[[3]]$next_step$fcst[3], is_q_predict[[4]]$next_step$fcst[3])

var_covid_q4 <- c(is_q_predict[[1]]$next_step$fcst[4], is_q_predict[[2]]$next_step$fcst[4],
              is_q_predict[[3]]$next_step$fcst[4], is_q_predict[[4]]$next_step$fcst[4])

var_covid_q5 <- c(is_q_predict[[1]]$next_step$fcst[5], is_q_predict[[2]]$next_step$fcst[5],
              is_q_predict[[3]]$next_step$fcst[5], is_q_predict[[4]]$next_step$fcst[5])


enet_covid_gini <- c(results[[1]]$predictions[21], results[[2]]$predictions[22], 
                results[[3]]$predictions[23], results[[4]]$predictions[24])

enet_covid_q1 <- c(q1_results[[1]]$predictions[21], q1_results[[2]]$predictions[22],
              q1_results[[3]]$predictions[23], q1_results[[4]]$predictions[24])

enet_covid_q2 <- c(q2_results[[1]]$predictions[21], q2_results[[2]]$predictions[22],
              q2_results[[3]]$predictions[23], q2_results[[4]]$predictions[24])

enet_covid_q3 <- c(q3_results[[1]]$predictions[21], q3_results[[2]]$predictions[22],
              q3_results[[3]]$predictions[23], q3_results[[4]]$predictions[24])

enet_covid_q4 <- c(q4_results[[1]]$predictions[21], q4_results[[2]]$predictions[22],
              q4_results[[3]]$predictions[23], q4_results[[4]]$predictions[24])

enet_covid_q5 <- c(q5_results[[1]]$predictions[21], q5_results[[2]]$predictions[22],
              q5_results[[3]]$predictions[23], q5_results[[4]]$predictions[24])

covid_rmse <- c(
  rmse_imp(observed = data$is_q1[21:24], var = var_covid_q1, enet = enet_covid_q1),
  rmse_imp(observed = data$is_q2[21:24], var = var_covid_q2, enet = enet_covid_q2),
  rmse_imp(observed = data$is_q3[21:24], var = var_covid_q3, enet = enet_covid_q3),
  rmse_imp(observed = data$is_q4[21:24], var = var_covid_q4, enet = enet_covid_q4),
  rmse_imp(observed = data$is_q5[21:24], var = var_covid_q5, enet = enet_covid_q5),
  rmse_imp(observed = data$gini[21:23], var = var_covid_gini[1:3] , enet = enet_covid_gini[1:3])
)


rmse_table <- rbind(
  cbind(q1_rmse_imp, q2_rmse_imp, 
        q3_rmse_imp, q4_rmse_imp, 
        q5_rmse_imp, gini_rmse_imp),
  covid_rmse) * 100

rmse_table <- round(rmse_table, 1)
rownames(rmse_table) <- c(paste0('Data Ending ', 2019:2022), 'Covid (2020-2022)')

# Replication for Second Panel ####
  # Gini
    gini_dat <- data.frame(
      year = data$year,
      true = data$gini,
      predictions_e19 = results[[1]]$predictions[,1],
      predictions_e20 = results[[2]]$predictions[,1],
      predictions_e21 = results[[3]]$predictions[,1],
      predictions_e22 = results[[4]]$predictions[,1]
    ) %>%
    mutate(across(-year, 
                  .fns = ~.x - lag(.x))) %>%
    mutate(across( - c(year, true),
                 .fns = ~.x * true)) %>%
    filter(year > 2000) %>%
    mutate(
      predictions_e19 = case_when(
        year %in% 2021:2023 ~ NA,
        TRUE ~ predictions_e19
        ),
      predictions_e20 = case_when(
        year %in% 2022:2023 ~ NA,
        TRUE ~ predictions_e20
        ),
      predictions_e21 = case_when(
        year %in% 2023 ~ NA,
        TRUE ~ predictions_e21)) %>%
  dplyr::select(-true) %>%
  mutate(across(-year,
                .fns = ~ifelse(.x >= 0, 1, 0))) %>%
  dplyr::select(-year) %>%
  summarise_all(hablar::mean_)





# Replication for Final Panel ####