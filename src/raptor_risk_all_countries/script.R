options(dplyr.summarise.inform = FALSE)

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

