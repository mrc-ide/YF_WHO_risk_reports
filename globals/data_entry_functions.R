### DATA ENTRY FUNCTIONS ###


#-----------------------------------------------------#
#' get list of provinces for a specific country
#' 
#' @param input_country country that is selected, 
#'                      this should be in the shape file
#' @return a data frame of provinces names and ids
#' @export
list_of_provinces = function(input_ISO){
  
  
  if(is.null(input_ISO)){safeError("Country not found")}
  
  #define temporary data fame
  tmp = data.frame(ISO = shp_file$ISO,
                   country = shp_file$NAME_0,
                   
                   province = shp_file$NAME_1,
                   province_id = shp_file$GID_1,
                   
                   district_id = shp_file$SPID,
                   district = shp_file$NAME_2)
  
  # filter to input country
  tmp %<>% dplyr::filter(ISO == input_ISO)
  
  return(tmp)
  
}

#-----------------------------------------------------#
#' make a template dataframe for a specific country
#' 
#' @param input_country country that is selected, 
#'                      this should be in the shape file
#' @return a data frame including country, province, province id, 
#'                      susepcted and confirmed cases
#' @export
make_template = function(input_country, input_admin = TRUE){
  
  if(input_admin){
    df = NULL
    
    for(i in 1:length(input_country)){
      
      current_country = input_country[i]
      
      input_ISO = list_of_ISOs[list_of_countries == current_country]
      
      if(is.null(current_country)){
        safeError("Country not found")
      }
      
      out = list_of_provinces(input_ISO)
      
      tmp_df = data.frame(Country = current_country,
                          
                          Provinces = out$province,
                          District_id = out$district_id,
                          District = out$district,
                          Suspected_cases = rep(0, length(out$province)),
                          Confirmed_cases = rep(0, length(out$province)),
                          Date_of_last_suspected_case = as.Date(rep(Sys.Date()), length(out$province)),
                          Date_of_last_confirmed_case = as.Date(rep(Sys.Date()), length(out$province)),
                          Population_immunity = rep(0, length(out$province)))
      
      
      tmp_df %<>% arrange(as.character(District_id))
      
      df %<>% bind_rows(tmp_df)
    }
  } else {
    df = NULL
    
    for(i in 1:length(input_country)){
      
      current_country = input_country[i]
      
      input_ISO = list_of_ISOs[list_of_countries == current_country]
      
      if(is.null(current_country)){
        safeError("Country not found")
      }
      
      out = list_of_provinces(input_ISO)
      
      tmp_df = data.frame(Country = current_country,
                          
                          Province = out$province,
                          Province_id = out$province_id,
                          Suspected_cases = rep(0, length(out$province)),
                          Confirmed_cases = rep(0, length(out$province)),
                          Date_of_last_suspected_case = as.Date(rep(Sys.Date()), length(out$province)),
                          Date_of_last_confirmed_case = as.Date(rep(Sys.Date()), length(out$province)),
                          Population_immunity = rep(0, length(out$province)))
      
      
      tmp_df %<>% arrange(as.character(Province_id)) %>% unique()
      
      df %<>% bind_rows(tmp_df)
    }
  }
  
  
  return(df)
}

#-----------------------------------------------------#
#' Import data after checking file type
#' 
#' @param input input of shiny app
#' 
#' @return imported data, unless: file type, variable names
#'         numeric cases, nonnegative cases or province checks fail, 
#'         then throw safe error
#' @export
get_upload = function(input){
  
  file_upload = input$file_in
  
  #if empty upload
  if(is.null(file_upload)){
    df = make_template(input$selected_country, input$admin_level)
  } else {
    
    #check correct file type. This works for both comma separated and semicolon separated Csv
    if(sum(grep(".csv", paste(file_upload$datapath) ))==0){
      stop(safeError("Wrong file type uploaded, file should be a .csv ."))
    }
    
    #import
    df = as.data.frame(fread(paste(file_upload$datapath), stringsAsFactors = FALSE, na.strings = ""))
    
    #run_all_checks
    df = run_checks(df)
  }
  
  return(df)
}

#-----------------------------------------------------#
#' Add YF coverage
#' 
#' @param data_entered tabe of data entered so far
#' @param input all inputs so far
#' 
#' @return updated data entered
#' @export  
#' 
add_yf_coverage <- function(data_entered, input){
  
  if((!input$admin_level & !"GID_2" %in% names(data_entered)) | !"District_id" %in% names(data_entered) ){
    
    data_entered %<>% mutate(Population_immunity = yf_coverage$coverage[match(data_entered$Province_id, yf_coverage$new_id)]) 
    
  } else {
    
    data_entered %<>% mutate(Population_immunity = yf_coverage$coverage[match(data_entered$District_id, yf_coverage$SPID)]) 
    
  }
  
  return(data_entered)
}