# running instructions#

lapply(c("Ethiopia", "Sudan", "Eritrea", "Kenya", "South Sudan", "Tanzania", "Uganda"),
       FUN=function(x)orderly::orderly_run("raptor_risk_per_country", 
                                           parameters = list(ctry=x, size_input=50, n_sample=1)))

orderly_run("raptor_risk_all_countries")
       
       
       