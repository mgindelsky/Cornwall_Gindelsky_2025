rm(list=ls())
pacman::p_unload(all)
pacman::p_load(vars, tidyverse, readxl, rstudioapi, forecast, readxl)

# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = '/var_models.R', replacement = '')

data <- read_excel(paste0(root,'/replication_data.xlsx'))

data <- data %>%
  dplyr::select(year, gini, contains('is_q')) 

gini_predict <- list()
is_q_predict <- list()
for (yr in 2020:2023) {
  temp_data <- data %>%
    filter(year <= yr) %>%
    dplyr::select(-year)
  #Gini AR2
  gini_mod <- forecast::Arima(temp_data$gini, order = c(2, 0, 0))
  #Quintiles VAR2
  is_q_mod <- vars::VAR(log(temp_data[,paste0('is_q',1:5)]), p = 2)
  gini_predict[[yr - 2019]] <- list(
    model = paste0('Data Ending ', yr),
    fitted = round(fitted.values(gini_mod),1),
    next_step = round(predict(gini_mod, n.ahead = 1)$pred,1)
  )
  q_predict <- predict(is_q_mod, n.ahead = 1)$fcst %>% 
    do.call(rbind,.) %>%
    as_tibble() %>% 
    dplyr::select(fcst)
  
  is_q_predict[[yr - 2019]] <- list(
    model = paste0('Data Ending ', yr),
    fitted = round(exp(fitted.values(is_q_mod))/rowSums(exp(fitted.values(is_q_mod)))*100,1),
    next_step = round(exp(q_predict)/sum(exp(q_predict))*100,1)
  )
}


vgini_dat <- data.frame(year = data$year,
                       true = data$gini,
                       predictions_e20 = c(NA, NA, gini_predict[[1]]$fitted[-c(1:2)], gini_predict[[1]]$next_step, NA, NA, NA),
                       predictions_e21 = c(NA, NA, gini_predict[[2]]$fitted[-c(1:2)], gini_predict[[2]]$next_step, NA, NA),
                       predictions_e22 = c(NA, NA, gini_predict[[3]]$fitted[-c(1:2)], gini_predict[[3]]$next_step, NA),
                       predictions_e23 = c(NA, NA, gini_predict[[4]]$fitted[-c(1:2)], gini_predict[[4]]$next_step))

vq1_dat <- data.frame(year = data$year,
                      true = data$is_q1,
                      predictions_e19 = c(NA, NA, is_q_predict[[1]]$fitted[,1], is_q_predict[[1]]$next_step$fcst[1], NA, NA, NA),
                      predictions_e20 = c(NA, NA, is_q_predict[[2]]$fitted[,1], is_q_predict[[2]]$next_step$fcst[1], NA, NA),
                      predictions_e21 = c(NA, NA, is_q_predict[[3]]$fitted[,1], is_q_predict[[3]]$next_step$fcst[1], NA),
                      predictions_e22 = c(NA, NA, is_q_predict[[4]]$fitted[,1], is_q_predict[[4]]$next_step$fcst[1]))

vq2_dat <- data.frame(year = data$year,
                      true = data$is_q2,
                      predictions_e19 = c(NA, NA, is_q_predict[[1]]$fitted[,2], is_q_predict[[1]]$next_step$fcst[2], NA, NA, NA),
                      predictions_e20 = c(NA, NA, is_q_predict[[2]]$fitted[,2], is_q_predict[[2]]$next_step$fcst[2], NA, NA),
                      predictions_e21 = c(NA, NA, is_q_predict[[3]]$fitted[,2], is_q_predict[[3]]$next_step$fcst[2], NA),
                      predictions_e22 = c(NA, NA, is_q_predict[[4]]$fitted[,2], is_q_predict[[4]]$next_step$fcst[2]))

vq3_dat <- data.frame(year = data$year,
                      true = data$is_q3,
                      predictions_e19 = c(NA, NA, is_q_predict[[1]]$fitted[,3], is_q_predict[[1]]$next_step$fcst[3], NA, NA, NA),
                      predictions_e20 = c(NA, NA, is_q_predict[[2]]$fitted[,3], is_q_predict[[2]]$next_step$fcst[3], NA, NA),
                      predictions_e21 = c(NA, NA, is_q_predict[[3]]$fitted[,3], is_q_predict[[3]]$next_step$fcst[3], NA),
                      predictions_e22 = c(NA, NA, is_q_predict[[4]]$fitted[,3], is_q_predict[[4]]$next_step$fcst[3]))

vq4_dat <- data.frame(year = data$year,
                      true = data$is_q4,
                      predictions_e19 = c(NA, NA, is_q_predict[[1]]$fitted[,4], is_q_predict[[1]]$next_step$fcst[4], NA, NA, NA),
                      predictions_e20 = c(NA, NA, is_q_predict[[2]]$fitted[,4], is_q_predict[[2]]$next_step$fcst[4], NA, NA),
                      predictions_e21 = c(NA, NA, is_q_predict[[3]]$fitted[,4], is_q_predict[[3]]$next_step$fcst[4], NA),
                      predictions_e22 = c(NA, NA, is_q_predict[[4]]$fitted[,4], is_q_predict[[4]]$next_step$fcst[4]))

vq5_dat <- data.frame(year = data$year,
                      true = data$is_q5,
                      predictions_e19 = c(NA, NA, is_q_predict[[1]]$fitted[,5], is_q_predict[[1]]$next_step$fcst[5], NA, NA, NA),
                      predictions_e20 = c(NA, NA, is_q_predict[[2]]$fitted[,5], is_q_predict[[2]]$next_step$fcst[5], NA, NA),
                      predictions_e21 = c(NA, NA, is_q_predict[[3]]$fitted[,5], is_q_predict[[3]]$next_step$fcst[5], NA),
                      predictions_e22 = c(NA, NA, is_q_predict[[4]]$fitted[,5], is_q_predict[[4]]$next_step$fcst[5]))

#Remove Intermediate Outputs and Leave Only Inputs, Final Outputs, and Directory Info ####
rm(list=setdiff(ls(), c("root","data", 
                        "vgini_dat", "vq1_dat",
                        "vq2_dat","vq3_dat","vq4_dat",
                        "vq5_dat")))


# Save list objects with results 
save(vgini_dat, vq1_dat, vq2_dat, vq3_dat, vq4_dat, vq5_dat,
     file = paste0(root, '/var_predict_results.Rda'))
 
