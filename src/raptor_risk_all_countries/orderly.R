orderly2::orderly_strict_mode()

orderly2::orderly_shared_resource("fun/run_risk_propagation.R" = "run_risk_propagation.R", "fun/risk_calc_functions.R" = "risk_calc_functions.R", "fun/plot_map_function.R" = "plot_map_function.R", "fun/fun_run_agg.R" = "fun_run_agg.R", "fun/fun_per_ctry.R" = "fun_per_ctry.R", "fun/data_source.R" = "data_source.R", "fun/data_entry_functions.R" = "data_entry_functions.R", "fun/checking_functions.R" = "checking_functions.R", "data/rad1.rds" = "data/rad1.rds", "data/rad2.rds" = "data/rad2.rds", "data/rad3.rds" = "data/rad3.rds", "data/rad4.rds" = "data/rad4.rds", "data/rad5.rds" = "data/rad5.rds", "data/rad6.rds" = "data/rad6.rds", "data/age_prop.csv" = "data/age_prop.csv", "data/YF_vac_cov.RDS" = "data/YF_vac_cov.RDS", "data/shp/population.rds" = "data/shp/population.rds", "data/shp/shp_file.rds" = "data/shp/shp_file.rds", "data/shp/shp_df.rds" = "data/shp/shp_df.rds", "data/shp/shp_file_outline.rds" = "data/shp/shp_file_outline.rds", "data/FOI_R0_med_values_af_yem_new.csv" = "data/FOI_R0_med_values_af_yem_new.csv")

orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Ethiopia')",
  c(location_starts_Ethiopia.rds = "location_starts.rds",
    risk_results_Ethiopia.rds = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Sudan')",
  c(location_starts_Sudan.rds = "location_starts.rds",
    risk_results_Sudan.rds = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Eritrea')",
  c(location_starts_Eritrea.rds = "location_starts.rds",
    risk_results_Eritrea.rds = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Kenya')",
  c(location_starts_Kenya.rds = "location_starts.rds",
    risk_results_Kenya.rds = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'South Sudan')",
  c("location_starts_South Sudan.rds" = "location_starts.rds",
    "risk_results_South Sudan.rds" = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Tanzania')",
  c(location_starts_Tanzania.rds = "location_starts.rds",
    risk_results_Tanzania.rds = "risk_results_extend.rds"))
orderly2::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Uganda')",
  c(location_starts_Uganda.rds = "location_starts.rds",
    risk_results_Uganda.rds = "risk_results_extend.rds"))

orderly2::orderly_artefact(
  "All figures",
  c("table_of_propn_nonzero_neighbour.png", 
    "map_risk_extend_mean_neighbours.png", 
    "map_risk_extend_mean_neighboursx2.png", 
    "map_risk_extend_median_neighbours.png", 
    "map_risk_extend_median_neighboursx2.png", 
    "map_start_locations_neighbours.png", 
    "map_start_locations_neighboursx2.png", 
    "plot_risk_extend_mean_neighbours.png", 
    "plot_risk_extend_mean_neighboursx2.png", 
    "plot_risk_extend_median_neighbours.png", 
    "plot_risk_extend_median_neighboursx2.png"))

orderly2::orderly_artefact(
  "All data",
  c("propn_zero_neighbours.rds", 
    "propn_zero_neighboursx2.rds",
    "all_DS_results_neighbours.rds",
    "all_DS_results_neighboursx2.rds"))

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

R.utils::sourceDirectory("fun", modifiedOnly=FALSE)

neighbours <- c("Eritrea", "Ethiopia", "Kenya")
neighboursx2 <- c("Ethiopia", "Sudan", "Eritrea", "Kenya", "South Sudan", "Tanzania", "Uganda")

fun_run_agg(list_of_ctrys = neighbours, out_name = "neighbours")

fun_run_agg(list_of_ctrys = neighboursx2, out_name = "neighboursx2")

propn_zero_neighbours <- readRDS("propn_zero_neighbours.rds")

propn_zero_neighbours %>%
  arrange(propn_zero) %>%
  head(n=20) %>%
  mutate(propn_zero = round(propn_zero*100,2)) %>%
  rename(`Proportion of simulations with zero risk (%)` = propn_zero) %>%
  flextable::flextable(cwidth = 2) %>%
  flextable::save_as_image(path = "table_of_propn_nonzero_neighbour.png")

