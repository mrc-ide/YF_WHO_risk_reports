fun_run_agg <- function(list_of_ctrys, out_name, pop=FALSE){
  # pull in relevant files to put together- starting locations
  loc_starts_fil <- list.files(pattern = "^location")
  loc_starts_fil <- loc_starts_fil[grepl(paste0(list_of_ctrys, collapse = "|"), loc_starts_fil)]
  
  loc_starts <- unlist(lapply(loc_starts_fil, readRDS))
  
  # - results
  results_fil <- list.files(pattern = "^risk_results")
  results_fil <- results_fil[grepl(paste0(list_of_ctrys, collapse = "|"), results_fil)]
  
  results <- bind_rows(lapply(results_fil, readRDS))
  
  # get shaepfile
  out  <-  fun_risk_propagate_generation(make_template(list_of_ctrys) %>% mutate(Confirmed_cases = 1), n_gen = 1)
  
  #-----------------------------------------------------------------------------#
  # make some plots
  ## MAPS OF RISK ##
  df_ave <- results %>% 
    group_by(Name, Province, District_id, District, population) %>% 
    summarise(risk_score = mean(risk_score, na.rm = TRUE))
  
  #plot the map
  png(filename = paste0("map_risk_extend_mean_",out_name,".png"), height = 1400, width=1400)
  report_plot_map_function( out$this_shape, df_ave, shp_file_outline, thresh = 0.1)
  dev.off()
  
  ## MAP OF POP ##
  if(pop){
    png(filename = paste0("map_pop_", out_name, ".png"), height = 1400, width=1400)
    report_plot_map_function( out$this_shape, df_ave, shp_file_outline, pop=TRUE)
    dev.off()
  }
  
  ## MEDIAN ##
  df_ave <- results %>% 
    group_by(Name, Province, District_id, District, population) %>% 
    summarise(risk_score = median(risk_score, na.rm = TRUE))
  
  #plot the map median
  png(filename = paste0("map_risk_extend_median_", out_name, ".png"), height = 1400, width=1400)
  report_plot_map_function( out$this_shape, df_ave, shp_file_outline, thresh= 0.1)
  dev.off()
  
  ## MAP THE START ##
  png(filename = paste0("map_start_locations_", out_name, ".png"), height = 1400, width=1400)
  start_loc_map_function( out$this_shape, loc_starts, shp_file_outline)
  dev.off()
  
  
  ## OCCURRENCE ##
  
  # plot the occurrence of each country
  df_sum <- results %>% group_by(start_loc, Name) %>% mutate(appears = sum(risk_score)>0,
                                                             n_dist = length(unique(District_id))) %>% 
    group_by(Name) %>% summarise(country_risk_ave = median(risk_score, na.rm = TRUE)/n_dist[1],
                                 n_appear = sum(appears)/n())
  
  p <- df_sum %>%
    filter( country_risk_ave>0) %>%
    mutate(is_ds= Name %in% c("Djibouti", "Somalia")) %>%
    ggplot()+
    aes(x =n_appear, y = country_risk_ave, colour = is_ds, label = Name )+
    geom_point()+
    geom_label(vjust = "inward", hjust = "inward")+
    theme_minimal()+
    labs(x = "Proportion of samples where country appears",
         y = "Median risk per country standardised by number of districts")+
    scale_color_manual(values = MetBrewer::met.brewer("Renoir", 7, type = "continuous")[c(1,5)])+
    theme(legend.position = "none", text=element_text(size=14))
  
  ggsave(plot=p, filename=paste0("plot_risk_extend_median_",out_name, ".png"), bg = "white")
  
  df_sum2 <- results %>% group_by(start_loc, Name) %>% mutate(appears = sum(risk_score)>0,
                                                              n_dist = length(unique(District_id))) %>% 
    group_by(Name) %>% summarise(country_risk_ave = mean(risk_score, na.rm = TRUE)/n_dist[1],
                                 n_appear = sum(appears)/n())
  
  df_sum2 <- df_sum2 %>% filter( country_risk_ave>1e-3)
  
  p2 <- df_sum2 %>%
    mutate(is_ds= Name %in% c("Djibouti", "Somalia")) %>%
    ggplot()+
    aes(x =n_appear, y = country_risk_ave, colour = is_ds, label = Name )+
    geom_point()+
    geom_label(vjust = "inward", hjust = "inward")+
    theme_minimal()+
    labs(x = "Proportion of samples where country appears",
         y = "Mean risk per country standardised by number of districts (risk>0.001)")+
    scale_color_manual(values = MetBrewer::met.brewer("Renoir", 7, type = "continuous")[c(1,5)])+
    theme(legend.position = "none", text=element_text(size=14))+
    scale_y_log10()
  
  ggsave(plot=p2, filename=paste0("plot_risk_extend_mean_",out_name, ".png"), bg = "white")
  
  # zeros
  results %>% 
    filter(Name %in% c("Djibouti", "Somalia")) %>% 
    group_by(Name, Province, District) %>% 
    summarise(propn_zero = sum(risk_score==0)/n()) %>%
    saveRDS(paste0("propn_zero_", out_name, ".rds"))
}