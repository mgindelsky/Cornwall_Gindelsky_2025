plot_results <- function(target_value,
                         results_list, 
                         ylab,
                         xlab = 'Year',
                         title,
                         dir = NULL,
                         sv_name = NULL) {
  
  #Inputs - 
    # target value: this is the published series
    # results_list: this is a list of dataframes with the predictions
    # ylab: the y-axis label
    # xlab: the x-axis label
    # title: the title of the plot
    # dir: the directory to save the plot (optional)
    # sv_name: the name of the plot (optional)
  
  #Output -
    # A plot of the published series and the predictions
    # If dir and sv_name are provided, the plot will be saved to the directory
  
  
  plot_data <- data.frame(
    year = seq(2000, by = 1, length.out = length(target_value)),
    target = target_value,
    de_2019 = as.vector(results_list[[1]]$predictions),
    de_2020 = as.vector(results_list[[2]]$predictions),
    de_2021 = as.vector(results_list[[3]]$predictions),
    de_2022 = as.vector(results_list[[4]]$predictions)
  ) %>%
    mutate(de_2019 = case_when(
      year > 2020 ~ NA,
      TRUE ~ de_2019
    )) %>%
    mutate(de_2020 = case_when(
      year > 2021 ~ NA,
      TRUE ~ de_2020
    )) %>%
    mutate(de_2021 = case_when(
      year > 2022 ~ NA,
      TRUE ~ de_2021
    )) %>%
    mutate(de_2022 = case_when(
      year > 2023 ~ NA,
      TRUE ~ de_2022
    )) %>%
    pivot_longer(-year,
                 names_to = 'model',
                 values_to = 'value') %>%
    mutate(model = case_when(
      model == 'target' ~ 'Published Series',
      model == 'de_2019' ~ 'Data Ending 2019',
      model == 'de_2020' ~ 'Data Ending 2020',
      model == 'de_2021' ~ 'Data Ending 2021',
      model == 'de_2022' ~ 'Data Ending 2022'
    ))
  
  plot <- ggplot(plot_data,
                 aes(x = year,
                     y = value, 
                     group = model, 
                     color = model,
                     linetype = model,
                     shape = model)) +
    theme_bw(base_size = 18) + 
    geom_line(linewidth = 1.1) + 
    geom_point(size = 4) + 
    theme(legend.position = 'bottom', 
          legend.text = element_text(face = 'bold', size = 18),
          axis.text.x = element_text(face = 'bold', size = 18),
          axis.text.y = element_text(face = 'bold', size = 18),
          axis.title.x = element_text(face = 'bold', size = 18),
          axis.title.y = element_text(face = 'bold', size = 18)) + 
    scale_color_manual(name = '',
                       values = c('Published Series' = 'black',
                                  'Data Ending 2019' = 'purple',
                                  'Data Ending 2020' = 'red',
                                  'Data Ending 2021' = 'blue',
                                  'Data Ending 2022' = 'yellow')) +
    scale_linetype_manual(name = '',
                          values = c('Published Series'  = 'solid',
                                     'Data Ending 2019' = 'dashed',
                                     'Data Ending 2020' = 'dotted',
                                     'Data Ending 2021' = 'dotdash',
                                     'Data Ending 2022' = 'twodash')) + 
    scale_shape_manual(name = '',
                       values = c('Published Series' = 15,
                                  'Data Ending 2019' = 16,
                                  'Data Ending 2020' = 17,
                                  'Data Ending 2021' = 16,
                                  'Data Ending 2022' = 18)) + 
    ylab(ylab) + xlab(xlab) + ggtitle(title)
  
  if (is.null(dir) | is.null(sv_name)) {
  } else {
    ggsave(filename = paste0(dir, '/', sv_name,'.png'),
           plot,
           width = 19,
           height = 9,
           dpi = 'retina')
  }
  
  return(suppressWarnings(print(plot)))
}
