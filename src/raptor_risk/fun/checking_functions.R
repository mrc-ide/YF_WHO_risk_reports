### CHECKING FUNCTIONS ###

#' check that all variable names are present
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return df, unless names are missing, then throw safe error
#' @export
check_variable_names = function(df, admin_level){
  
  var_names = names(make_template("Nigeria", admin_level))
  
  if(sum(!var_names %in% names(df))>0){
    stop(safeError("There are names missing from the upload, please see template."))
  }
  
  return(df)
}

#----------------------------------------------#
#' check entered cases are a numeric type
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return nothing, unless cases aren't numeric, then throw safe error
#' @export
check_numeric = function(df){
  ind = grep("cases", names(df))
  
  for(i in ind){
    if(!is.numeric(df[,i])){
      stop(safeError("There are non numeric entries in the cases."))
    }
  }
}

#----------------------------------------------#
#' check entered cases are a date type
#' 
#' @param vec column of uploaded data
#' 
#' @return vec in date format or an error
#' @export
assert_date = function(vec){
  
  vec_out = ymd(vec, quiet = TRUE)
  
  if(all(is.na(vec_out))){
    vec_out = dmy(vec, quiet = TRUE)
  }
  
  if(all(is.na(vec_out))){
    vec_out = mdy(vec, quiet = TRUE)
  }
  
  if(all(is.na(vec_out))){ 
    stop(safeError("Dates failed, please 1 of the following formats : dd/mm/yy, mm/dd/yy or yy/mm/dd"))
  }
  
  return(vec_out)
}
#----------------------------------------------#
#' check entered cases are a date type
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return df, unless cases aren't numeric, then throw safe error
#' @export
check_dates = function(df){
  ind = grep("Date", names(df))
  
  for(i in ind){
    df[, i] = assert_date(df[, i])
  }
  
  return(df)
}
#----------------------------------------------#
#' check cases are nonegative
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return nothing, unless cases are negative, then throw safe error
#' @export
check_nonnegative = function(df){
  ind = grep("cases", names(df))
  
  for(i in ind){
    if(min(df[,i])<0){
      stop(safeError("There are negative entries in the cases."))
    }
  }
}
#----------------------------------------------#
#' check provinces are all present
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return nothing, unless names are missing, then throw safe error
#' @export
check_provinces = function(df){
  country = df$Country[1]
  
  tmp = make_template(as.character(country))
  
  if(sum(!tmp$Province_id %in% df$Province_id)>0){
    stop(safeError("There are provinces missing from the entered data"))
  }
}

#----------------------------------------------#
#' run all checks
#' 
#' @param df uploaded dataframe of cases
#' 
#' @return df or errors
#' @export
run_checks = function(df){
  #check all varible names are present
  #df = check_variable_names(df)
  
  #check cases are numeric
  check_numeric(df)
  
  #check dates are dates
  df = check_dates(df)
  
  # check cases are nonnegative
  check_nonnegative(df)
  
  #check all provinces are there (this is done on id)
  # check_provinces(df)
  
  return(df)
}


#----------------------------------------------#
#' check utf-8
#' 
#' @param df data frame
#' 
#' @return df with utf 8  cnversion
#' @export
check_utf8 = function(df){
  
  for(i in 1:ncol(df)){
    df[, i] =  stringi::stri_trans_general(str = df[, i], id = "Latin-ASCII")
  }
  
  
  #TEST
  tmp <- tempfile()
  for(i in 1:ncol(df)){
    xfun::write_utf8(df[,i], tmp)
  }
  file.remove(tmp)
  return(df)
}