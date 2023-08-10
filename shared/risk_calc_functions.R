### RISK CALCULATION FUNCTIONS ###

#----------------------------------------------#

#----------------------------------------------#
#' function returning a data.frame with the total risk score and its components
#' 
#' 
#' @inheritParams pop
#' @inheritParams vc
#' @inheritParams inci
#' @inheritParams mov
#' @inheritParams T_E
#' @inheritParams T_I
#' @inheritParams W
#' @inheritParams vac_eff
#' @inheritParams risk_pop
#' 
#' @return resulting risk scores in data frame
#' @export
main_risk_function = function(pop, 
                              vc, 
                              inci, 
                              mov,
                              vac_eff,
                              risk_pop){
  
  inci = as(inci, "sparseVector")
  mov = as(mov, "sparseMatrix")
  
  intro_risk = inci %*% mov
  
  pop_at_risk = 0
  alpha = -0.1
  while(pop_at_risk < (risk_pop*1000) & alpha < 1e3 ){
    
    alpha = alpha + 0.1 #so effectively starting at 0
    
    risk_tmp = alpha*(1-vc)*intro_risk
    
    pop_at_risk = sum( (risk_tmp >= 0.1)*pop)
    
    
  }
  
  return(alpha*(1-vc)*intro_risk)
}


#----------------------------------------------------#
#' function returning a data.frame in useful format of risk scores
#' 
#' @param df uploaded or entered incidence data- reactive variable
#' @param input shiny input values
#' 
#' @return resulting risk scores in data frame, shapefile for country of interest
#' @export
make_risk_df = function(data_entered, cross_border_scaling = 1){
  
  selected_country = unique(data_entered$Country)
  
  #----------------------------------#
  # get outbreak duration
  data_entered = check_dates(data_entered)
  
  
  date_ind = grep("Date", names(data_entered))
  date_vec = as.matrix(data_entered[, date_ind])
  dates <- assert_date(c(min(date_vec, na.rm = TRUE), 
                         max(date_vec, na.rm = TRUE)))
  
  
  #----------------------------------#
  #Get data from suspected and confirmed cases
  sus_conf_cases<-data_entered %>% 
    mutate(Suspected_cases = as.numeric(as.character(Suspected_cases)),
           Confirmed_cases = as.numeric(as.character(Confirmed_cases)))
  
  sus_conf_cases %<>% mutate(District_id = as.character(District_id))
  
  #case definition to use
  sus_conf_cases$inci = rowSums(sus_conf_cases[, c("Suspected_cases", "Confirmed_cases")])
  
  sus_conf_cases$inci[is.na(sus_conf_cases$inci)] <- 0
  
  if(sum(sus_conf_cases$inci)==0) stop(safeError("No cases entered"))
  
  #----------------------------------#
  # ignore cases before the date_range input
  sus_conf_cases$date_min = assert_date( apply(sus_conf_cases[, date_ind], 1, min) )
  
  date_start = dates[1] 
  
  sus_conf_cases$inci[sus_conf_cases$date_min < date_start] = 0
  
  #----------------------------------#
  #Expand incidence df to the same size as the shapefile
  expanded_inci<-data.frame(ISO = shp_file$ISO,
                            SPID = shp_file$SPID)
  expanded_inci %<>% mutate(SPID = as.character(SPID))
  
  expanded_inci %<>% left_join(sus_conf_cases, by = c("SPID" = "District_id"))
  expanded_inci$inci[is.na(expanded_inci$inci)] = 0
  expanded_inci$Population_immunity[is.na(expanded_inci$Population_immunity)] = 0
  
  #----------------------------------#
  # add population #
  
  expanded_inci %<>% left_join( dplyr::select(population, c(SPID, population)), by = "SPID")
  
  #----------------------------------#
  # load movement 
  move_use <- radiation #gravity
  
  move_use = move_use[rownames(move_use) %in% shp_file$SPID, colnames(move_use) %in% shp_file$SPID]
  
  #----------------------------------#
  # scale cross border movement
  
  # get ISOs of cases
  ISO_internal = unique(substr( sus_conf_cases$District_id[ sus_conf_cases$inci >0], 1,3))
  
  
  # get external districts
  district_external_index = Diagonal(x = cross_border_scaling, n= ncol(move_use)) 
  diag(district_external_index)[grep(paste0(ISO_internal, collapse = "|"), rownames(move_use))] = 1
  
  #scale external 
  move_use = move_use %*% district_external_index
  
  #----------------------------------#
  # subset everything by movement
  ISO_use <- unique(substr( rownames(move_use), 1,3))
  
  SP_ID_USE = colnames(move_use)
  
  
  #Re-subset the incidence and shapefile based on this new distance probability
  expanded_inci_subset <- expanded_inci %>% filter(SPID %in% SP_ID_USE)
  
  this_shapefile <- shp_file[shp_file$ISO %in% ISO_use, ]
  this_shapefile_subset <- shp_file[shp_file$SPID %in% SP_ID_USE, ] 
  this_shapefile_subset$NAME_2[is.na(this_shapefile_subset$NAME_2)] = this_shapefile_subset$NAME_1[is.na(this_shapefile_subset$NAME_2)]
  
  # uniqueness
  ind = rownames(move_use) %in% expanded_inci_subset$SPID
  move_use = move_use[ind, ind]
  
  #-----------------------------------#
  #age group proportion
  age_prop_subset <- age_prop %>% 
    # filter(age<=70) %>% 
    group_by(ISO) %>% 
    summarise(prop = sum(prop)) 
  
  age_prop_subset %<>% mutate_if(is.factor, as.character)
  expanded_inci_subset %<>% mutate_if(is.factor, as.character)
  
  expanded_inci_subset %<>% left_join(age_prop_subset, by="ISO")
  
  #-----------------------------------#
  
  this_risk_score <- main_risk_function(vc = expanded_inci_subset$Population_immunity,
                                        inci = expanded_inci_subset$inci,
                                        pop = expanded_inci_subset$population * 1,#expanded_inci_subset$prop,
                                        mov = move_use,
                                        risk_pop = 100000,#sum(expanded_inci_subset[which(expanded_inci_subset$ISO %in% c("ETH", "DJI", "ERI", "SDN", "KEN", "SOM", "SSD")), ]$population),
                                        vac_eff = 1)
  
  this_risk_score[is.na(this_risk_score)|is.infinite(this_risk_score)] = 0
  
  #-----------------------------------#
  # set risk in placeswih cases to be the maximum
  this_risk_score[expanded_inci_subset$inci>0] = max(this_risk_score) + 
    expanded_inci_subset$inci[expanded_inci_subset$inci>0]
  
  #-----------------------------------#
  risk_df <- data.frame(Name = this_shapefile_subset$NAME_0,
                        Province = this_shapefile_subset$NAME_1,
                        District_id = expanded_inci_subset$SPID,
                        District = this_shapefile_subset$NAME_2,
                        Cases = expanded_inci_subset$inci,
                        population = expanded_inci_subset$population * 1,#expanded_inci_subset$prop,
                        vaccine_coverage = round(expanded_inci_subset$Population_immunity*100, 1),
                        risk_score = as(this_risk_score, "vector"),
                        stringsAsFactors = FALSE)
  
  
  return(list(risk_df = risk_df, this_shapefile = this_shapefile_subset))
}

