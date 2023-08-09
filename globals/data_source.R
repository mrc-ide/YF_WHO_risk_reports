population <- readRDS("data/shp/population.rds")
shp_file <- readRDS("data/shp/shp_file.rds")
shp_df <- readRDS("data/shp/shp_df.rds")
shp_file_outline <- readRDS("data/shp/shp_file_outline.rds")

shp_file@data$NAME_2[shp_file@data$NAME_2=="Bujenje (?)"] <- "Buliisa"

list_of_countries <-  unique(shp_file$NAME_0 ) 
list_of_ISOs <-  unique(shp_file$ISO) 

radiation = rbind(readRDS("data/rad1.rds"), readRDS("data/rad2.rds"), readRDS("data/rad3.rds"), 
                  readRDS("data/rad4.rds"), readRDS("data/rad5.rds"), readRDS("data/rad6.rds"))
 #----------------------------------------------
age_prop <- read.csv("data/age_prop.csv", stringsAsFactors = FALSE)

#----------------------------------------------
yf_coverage <- readRDS("data/YF_vac_cov.RDS")