
fun_risk_propagate <- function(dummy_data){
  #get risk everywhere
  risk_df_list <- make_risk_df(dummy_data)
  risk_df <- risk_df_list$risk_df
  this_shape <- risk_df_list$this_shapefile
  # get top 10% of risk areas
  risk_df <- risk_df %>% mutate(top10 = risk_score > quantile(risk_score, 0.9) )
  # add in cases
  ## first scale current cases
  dummy_data <- dummy_data %>% rowwise() %>% mutate(Confirmed_cases = Confirmed_cases*sample(c(0.75, 1, 1.25), 1, replace = TRUE))
  ## rescale risk
  risk_df <- risk_df %>% mutate(in_dummy = District_id %in% dummy_data$District_id)
  risk_df <- risk_df %>% group_by(in_dummy) %>% mutate(rescale_risk =( risk_score-min(risk_score, na.rm = TRUE))/
                                                         (max(risk_score, na.rm = TRUE)-min(risk_score, na.rm = TRUE) ))
  risk_df <- risk_df %>% ungroup() %>% filter(in_dummy, top10)
  
  ## work out what cases to add where
  risk_df <- risk_df %>% 
    rowwise() %>% 
    mutate(cases_to_add = sample(c(rep(0, 4), 0.25, 0.5, 0.75, 1), 1, replace = TRUE)*rescale_risk*10) %>%
    mutate(cases_to_add = plyr::round_any(cases_to_add, 1, ceiling))
  
  ## finally add
  dummy_data <- dummy_data %>%
    left_join(risk_df %>% ungroup() %>% dplyr::select(District_id, cases_to_add), by = "District_id") %>%
    mutate(Confirmed_cases = Confirmed_cases + cases_to_add) %>%
    dplyr::select(-cases_to_add) %>%
    ungroup()
  
  return(list(dummy_data=dummy_data, this_shape))
}

fun_risk_propagate_generation <- function(dummy_data, n_gen=1){
  if(n_gen>1){
    for(i in 1:n_gen-1){
      out <- fun_risk_propagate(dummy_data)
      dummy_data <- out$dummy_data %>% as.data.frame()
    }
  } 
  #then calculate risk from final
  risk_df_list <- make_risk_df(dummy_data)
  
  return(list(risk_df=risk_df_list$risk_df, this_shape = risk_df_list$this_shapefile))
}
