plot_map_function <- function(shp_file, risk_df, shp_file_outline){
  
  #SUbset to country
  country<-shp_file
  
  outline<-shp_file_outline[shp_file_outline$ISO %in% country$ISO, ]
  
  #Colour scheme for legend
  factors_add<-sort(as.factor(c("High", "Medium", "Low")))
  factor_legend_cols<-factors_add
  levels(factors_add)<-c("High", "Medium", "Low")
  levels(factor_legend_cols)<-c("Low", "Medium", "High")
  
  catpal<-colorFactor("YlOrRd", factor_legend_cols)
  
  #Subset to things that arent to be displayed
  risk_df_non_zero <- risk_df %>% filter(risk_score>=0.1)
  country_non_zero<-country[country$SPID %in% risk_df_non_zero$District_id, ]
  country_non_zero<-country_non_zero[order(country_non_zero$NAME_2), ]
  risk_df_non_zero<-risk_df_non_zero[order(risk_df_non_zero$District), ]
  
  #Creating the popup from the risk df
  popup<-paste("Name: ", risk_df_non_zero$District, 
               "<br/>Population: ", formatC(risk_df_non_zero$population, format = "d", big.mark = ","), 
               "<br/>Population immunity: ", paste0(risk_df_non_zero$vaccine_coverage, " %"),
               "<br/>Cases: ", risk_df_non_zero$Cases,
               "<br/>Risk score: ", round(risk_df_non_zero$risk_score, 2))
  
  
  #Colour scheme for plotting
  normal_colour<- log10(risk_df_non_zero$risk_score)
  numpal<-colorNumeric("YlOrRd", 
                       domain = c(min(normal_colour[is.finite(normal_colour)], na.rm = TRUE), 
                                  max(normal_colour[!is.infinite(normal_colour)], na.rm = TRUE)), 
                       na.color = "#ffffcc", 
                       alpha = FALSE)
  
  bounds = st_bbox(country_non_zero)
  
  #Plotting function
  leaflet(data = country_non_zero) %>%
    clearGroup("polys") %>%
    clearControls() %>%
    addPolygons(stroke=TRUE, fillOpacity = 1, smoothFactor=1,
                fillColor = ~numpal(normal_colour), col="black",
                weight=2, popup = popup, group = "polys")  %>%
    addLegend("bottomright", 
              colors = alpha("white", 1), 
              title = paste0("Population at risk"), 
              labels = formatC(round(sum(risk_df_non_zero$population), 0), 
                               format = "d", big.mark = ","),
              group = "pop_at_risk") %>%
    
    addLegend("bottomright", colors = rev(brewer.pal(9, "YlOrRd")), 
              labels = c("High", rep("", 7), "Low"), 
              title = "Relative risk score", opacity = 1)%>%
    fitBounds(as.numeric(bounds[1]), as.numeric(bounds[2]), as.numeric(bounds[3]), as.numeric(bounds[4]) )
  
}

report_plot_map_function<-function(shp_file, risk_df, shp_file_outline, thresh = 0.1, pop=FALSE){
  
  #SUbset to country
  country<-shp_file
  outline<-shp_file_outline[shp_file_outline$ISO %in% country$ISO, ]
  
  if(pop){thresh=1e-2}
  
  #Subset to things that arent to be displayed
  risk_df_non_zero <- risk_df %>% filter(risk_score>=thresh)
  country_non_zero<-country[country$SPID %in% risk_df_non_zero$District_id, ]
  country_non_zero<-country_non_zero[order(country_non_zero$NAME_2), ]
  risk_df_non_zero<-risk_df_non_zero[order(risk_df_non_zero$District), ]
  
  
  
  # map
  if(pop){
    parm = risk_df_non_zero$population
  } else {
    parm = log10(risk_df_non_zero$risk_score)
  }
  
  mybreaks= seq(min(parm, na.rm = TRUE)-1e-5, 
                max(parm, na.rm = TRUE)+1e-5,
                length.out=1000)
  
  if(!pop){
    mycols = colorRampPalette( brewer.pal(n = 9, name = "YlOrRd"))(length(mybreaks)-1)
  } else {
    mycols = colorRampPalette( brewer.pal(n = 9, name = "Greens"))(length(mybreaks)-1)
  }
  bounds = st_bbox(country_non_zero)
  
  mm = match(country_non_zero$SPID, risk_df_non_zero$District_id)
  
  vcols = findInterval(parm,mybreaks)
  
  plot(country_non_zero)
  plot(country_non_zero[!is.na(mm),], col=mycols[vcols] , lty=0, add=TRUE)
  plot(outline, lwd=2, add=TRUE)
  text(coordinates(outline), labels = outline$NAME_0)
  
  
}

start_loc_map_function<-function(shp_file, loc, shp_file_outline){
  
  #SUbset to country
  country<-shp_file
  outline<-shp_file_outline[shp_file_outline$ISO %in% country$ISO, ]
  
  
  #table loc
  l <- table(loc) %>% data.frame() %>% rename(District_id = 1)
  
  
  #Subset to things that arent to be displayed
  country_non_zero<-country[country$SPID %in% l$District_id, ]
  country_non_zero<-country_non_zero[order(country_non_zero$NAME_2), ]
  
  
  
  # map
  parm = log10(l[,2])
  
  mybreaks= seq(min(parm, na.rm = TRUE)-1e-5, 
                max(parm, na.rm = TRUE)+1e-5,
                length.out=1000)
  
  mycols = colorRampPalette( brewer.pal(n = 9, name = "Purples")[5:9])(length(mybreaks)-1)
  
  bounds = st_bbox(country_non_zero)
  
  mm = match(country_non_zero$SPID, l$District_id)
  
  vcols = findInterval(parm,mybreaks)
  
  plot(country_non_zero)
  plot(country_non_zero[!is.na(mm),], col=mycols[vcols] , lty=0, add=TRUE)
  plot(outline, lwd=2, add=TRUE)
  text(coordinates(outline), labels = outline$NAME_0)
  
  
}

