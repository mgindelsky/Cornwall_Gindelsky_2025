# First "results" program file needs to be run in order to generate the dataframes. Those datafames can then be used to generate Figure 1
# Load Packages ####
rm(list=ls())
pacman::p_unload(all)
pacman::p_load(readxl, tidyverse, ggplot2, glmnet, rstudioapi, pracma)

# Directories ####
root <- getSourceEditorContext()$path %>%    
  gsub(pattern = 'figure.R', replacement = '')

# Helper Function and data####
data <- read_excel(paste0(root,'/replication_data.xlsx'))
targets <- data %>%
  dplyr::select(year, gini,	is_q1,	is_q2,	is_q3,	is_q4,	is_q5)
load(paste0(root, 'plot_results.Rda'))
source(paste0(root, '/plot_results_function.R'))

#Gini
  plot_results(target_value = targets$gini, 
               results_list = results,
               ylab = 'Gini Coefficient',
               title = 'Nowcast of Gini Coefficient',
               dir = NULL,
               sv_name = NULL)
  #Q1
  plot_results(target_value = targets$is_q1,
               results_list = q1_results,
               ylab = 'Share of PI',
               title = 'Nowcast of Income Share: Q1',
               dir = NULL,
               sv_name = NULL)

  #Q2
  plot_results(target_value = targets$is_q2,
               results_list = q2_results,
               ylab = 'Share of PI',
               title = 'Nowcast of Income Share: Q2',
               dir = NULL,
               sv_name = NULL)
  
  #Q3
  plot_results(target_value = targets$is_q3,
               results_list = q3_results,
               ylab = 'Share of PI',
               title = 'Nowcast of Income Share: Q3',
               dir = NULL,
               sv_name = NULL)
  
  #Q4
  plot_results(target_value = targets$is_q4,
               results_list = q4_results,
               ylab = 'Share of PI',
               title = 'Nowcast of Income Share: Q4',
               dir = NULL,
               sv_name = NULL)
  
  #Q5
  plot_results(target_value = targets$is_q5,
               results_list = q5_results,
               ylab = 'Share of PI',
               title = 'Nowcast of Income Share: Q5',
               dir = NULL,
               sv_name = NULL)
  


