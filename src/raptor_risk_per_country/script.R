options(dplyr.summarise.inform = FALSE)

R.utils::sourceDirectory("fun", modifiedOnly = FALSE)

in_template <- make_template(c("Ethiopia", "Sudan",
                               "Eritrea", "Djibouti", "Kenya", 
                               "South Sudan", "Somalia", "Rwanda", "Burundi", "Tanzania", "Uganda"))

in_template <- in_template %>% mutate(region_id = gsub("_$", "", gsub('.{2}$', '', District_id)))

foi <- read.csv("data/FOI_R0_med_values_af_yem.csv", stringsAsFactors = FALSE)
foi <- foi %>% mutate(region_id = gsub("\\.", "_", gsub('.{2}$', '', regions)))

# filter foi to template
foi <- foi %>% filter(region_id %in% in_template$region_id)

# join 'em up
in_template <- in_template %>% left_join(foi %>% dplyr::select(region_id, FOI_med), by = "region_id")
in_template <- in_template %>% mutate(FOI_med = ifelse(is.na(FOI_med), 0, FOI_med))

fun_make_output_country_start(in_template, foi, ctry, size_input, n_sample)
