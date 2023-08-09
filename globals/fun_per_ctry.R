fun_make_output_country_start <- function(in_template, foi, ctry, size_input = 100, n_sample = 1){
  #sample locations according to foi
  in_template_ctry <- in_template %>% filter(Country == ctry)
  
  sample_loc <- sample(in_template_ctry$District_id, prob = in_template_ctry$FOI_med, size = size_input, replace = TRUE)
  
  saveRDS(sample_loc, paste0("location_starts.rds"))
  
  df_out <- NULL
  #run for all locations
  for(i in seq_along(sample_loc)){
    # run for each location
    for(j in 1:n_sample){
      dummy_data <- in_template
      
      #add 10 infections to location
      dummy_data <- dummy_data %>% mutate(Confirmed_cases = ifelse(District_id %in% sample_loc[i],
                                                                   10,
                                                                   0))
      
      out  <-  fun_risk_propagate_generation(dummy_data, n_gen = 1)
      df_out <- df_out %>% bind_rows(out$risk_df %>% mutate(start_loc = sample_loc[i]))
    }
  }
  
  
  df_ave <- df_out %>% 
    group_by(Name, Province, District_id, District, population) %>% 
    summarise(risk_score = mean(risk_score, na.rm = TRUE))
  
  #plot the map
  png(filename = paste0("map_risk_extend_mean.png"), height = 1400, width=1400)
  report_plot_map_function( out$this_shape, df_ave, shp_file_outline)
  dev.off()
  
  df_ave <- df_out %>% 
    group_by(Name, Province, District_id, District, population) %>% 
    summarise(risk_score = median(risk_score, na.rm = TRUE))
  
  #plot the map median
  png(filename = paste0("map_risk_extend_median.png"), height = 1400, width=1400)
  report_plot_map_function( out$this_shape, df_ave, shp_file_outline)
  dev.off()
  
  # plot the occurrence of each country
  df_sum <- df_out %>% group_by(start_loc, Name) %>% mutate(appears = sum(risk_score)>0,
                                                            n_dist = length(unique(District_id))) %>% 
    group_by(Name) %>% summarise(country_risk_ave = median(risk_score, na.rm = TRUE)/n_dist[1],
                                 n_appear = sum(appears)/n())
  
  df_sum %>%
    filter( country_risk_ave>0) %>%
    ggplot()+
    aes(x =n_appear, y = country_risk_ave, colour = Name, label = Name )+
    geom_point()+
    geom_label(vjust = "inward", hjust = "inward", nudge_x = 1e-3)+
    theme_minimal()+
    labs(x = "Proportion of samples where country appears",
         y = "Median risk per country standardised by number of districts")+
    MetBrewer::scale_color_met_d(name = "Renoir")+
    theme(legend.position = "none")
  
  ggsave(paste0("plot_risk_extend_median.png"), bg = "white")
  
  df_sum2 <- df_out %>% group_by(start_loc, Name) %>% mutate(appears = sum(risk_score)>0,
                                                             n_dist = length(unique(District_id))) %>% 
    group_by(Name) %>% summarise(country_risk_ave = mean(risk_score, na.rm = TRUE)/n_dist[1],
                                 n_appear = sum(appears)/n())
  
  
  df_sum2 %>%
    filter( country_risk_ave>0) %>%
    ggplot()+
    aes(x =n_appear, y = country_risk_ave, colour = Name, label = Name )+
    geom_point()+
    geom_label(vjust = "inward", hjust = "inward", nudge_x = 1e-3)+
    theme_minimal()+
    labs(x = "Proportion of samples where country appears",
         y = "Mean risk per country standardised by number of districts")+
    MetBrewer::scale_color_met_d(name = "Renoir")+
    theme(legend.position = "none")+
    scale_y_log10()
  
  ggsave(paste0("plot_risk_extend_mean.png"), bg = "white")
  
  saveRDS(df_out, paste0("risk_results_extend.rds"))
  
}