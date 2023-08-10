orderly2::orderly_strict_mode()

orderly2::orderly_parameters(ctry = "Ethiopia", size_input = 50L, n_sample = 10L)

orderly2::orderly_shared_resource("fun/run_risk_propagation.R" = "run_risk_propagation.R", "fun/risk_calc_functions.R" = "risk_calc_functions.R", "fun/plot_map_function.R" = "plot_map_function.R", "fun/fun_run_agg.R" = "fun_run_agg.R", "fun/fun_per_ctry.R" = "fun_per_ctry.R", "fun/data_source.R" = "data_source.R", "fun/data_entry_functions.R" = "data_entry_functions.R", "fun/checking_functions.R" = "checking_functions.R", "data/rad1.rds" = "data/rad1.rds", "data/rad2.rds" = "data/rad2.rds", "data/rad3.rds" = "data/rad3.rds", "data/rad4.rds" = "data/rad4.rds", "data/rad5.rds" = "data/rad5.rds", "data/rad6.rds" = "data/rad6.rds", "data/age_prop.csv" = "data/age_prop.csv", "data/YF_vac_cov.RDS" = "data/YF_vac_cov.RDS", "data/shp/population.rds" = "data/shp/population.rds", "data/shp/shp_file.rds" = "data/shp/shp_file.rds", "data/shp/shp_df.rds" = "data/shp/shp_df.rds", "data/shp/shp_file_outline.rds" = "data/shp/shp_file_outline.rds", "data/FOI_R0_med_values_af_yem_new.csv" = "data/FOI_R0_med_values_af_yem_new.csv")

orderly2::orderly_artefact(
  "All data",
  c("location_starts.rds", "risk_results_extend.rds"))
orderly2::orderly_artefact(
  "All figures",
  c("map_risk_extend_mean.png", "map_risk_extend_median.png", "plot_risk_extend_mean.png", "plot_risk_extend_median.png"))

library(data.table)
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sp)
library(Matrix)
library(magrittr)
library(lubridate)
library(RColorBrewer)
library(sf)

options(dplyr.summarise.inform = FALSE)

R.utils::sourceDirectory("fun", modifiedOnly = FALSE)

in_template <- make_template(c("Ethiopia", "Sudan",
                               "Eritrea", "Djibouti", "Kenya", 
                               "South Sudan", "Somalia", "Rwanda", "Burundi", "Tanzania", "Uganda"))

in_template <- in_template %>% mutate(region_id = gsub("_$", "", gsub('.{2}$', '', District_id)))

foi <- read.csv("data/FOI_R0_med_values_af_yem_new.csv", stringsAsFactors = FALSE)
foi <- foi %>% mutate(region_id = gsub("\\.", "_", gsub('.{2}$', '', region)))

# filter foi to template
foi <- foi %>% filter(region_id %in% in_template$region_id)

# join 'em up
in_template <- in_template %>% left_join(foi %>% dplyr::select(region_id, FOI_50), by = "region_id")
in_template <- in_template %>% mutate(FOI_med = ifelse(is.na(FOI_50), 0, FOI_50))

fun_make_output_country_start(in_template, foi, ctry, size_input, n_sample)
