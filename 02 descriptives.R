##############################################################################
## Descriotive statitics 
###############################################################################


## This file includes the descriptive statistics for the variables used in the 
# anslyisis. For ease I will be plotting three groups of variables in seperate 
# tables: 1) Subject characteristics, 2) Incident characteristics and 3) neighborhood 
# Characteristics. This way it will be possible to tell a story as a walk though
# covering the people, the events and where they happeneed.



## Load Packeges

library(modelsummary)
library(flextable)


tab <- trr %>%
  dplyr::select(
    `Force` = force,
    `% Black` = pct_af_am,
    `% Hispanic` = pct_hisp,
    `% In Poverty` = pct_pov,
    `Total Polulation` = t_pop,
    `Subject Race` = subject_race,
    `Resisiance` = resistence,
    `Homicide RateRate per 1000` = crime_rate
  ) %>% 
  datasummary_balance(~1, data = ., output = "data.frame") 

# remove unwanted cols 
#tab <- tab[,-c(2,3,7,9)] 

# Create a flextable 
tab <- tab |> flextable::flextable() 

tab
