orderly::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Ethiopia')",
  c(location_starts_Ethiopia.rds = "location_starts.rds",
    risk_results_Ethiopia.rds = "risk_results_extend.rds"))
orderly::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Eritrea')",
  c(location_starts_Eritrea.rds = "location_starts.rds",
    risk_results_Eritrea.rds = "risk_results_extend.rds"))
orderly::orderly_dependency(
  "raptor_risk_per_country",
  "latest(parameter:ctry == 'Kenya')",
  c(location_starts_Kenya.rds = "location_starts.rds",
    risk_results_Kenya.rds = "risk_results_extend.rds"))

orderly::orderly_artefact(description="combined start locations by WHO admin1",
                          files = "start_loc.rds")

orderly::orderly_resource("xref_adm1_6countries.Rds")

#-------------------------------------------------------------------------------
library(dplyr)

df <- c(sapply(list.files(pattern="location_starts"), readRDS))
xref <- readRDS("xref_adm1_6countries.Rds")

df <- data.frame(start_loc = df)
df <- df %>% rowwise() %>% mutate(start_adm1 = paste0(paste(strsplit(start_loc, "_")[[1]][1:2], collapse = "."), "_1"))
df$WHO_name <-  xref$WHO_Name[ match(df$start_adm1, xref$GADM_ID)]

df %>% group_by(WHO_name) %>% summarise(count = n()) %>% saveRDS("start_loc.rds")
