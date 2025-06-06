##########################################################################
### 401-2 Catagirical Regression Methods | Final Paper Data Clean V 2
##########################################################################

## Steps 

# Load in the data 
# merge force data onto an incident level 
# combine force data with additional datasets 
# final clean 




##########################################################################
### Loading In Datasets
##########################################################################


library(tidyverse)

# TRR data
load("../401-2 Catagorical Regression Final Paper/Data/trr_main.rds") 

# Description: Main information on tactical response reports (TRR) from January 
# 2004 through April 12, 2016. One TRR is filed per officer per subject per event. 
# TRRs include all physical actions (e.g. wrist locks), chemical weapons, tasers, 
# and shootings with firearms.

## Details 
# TRR level data: one row pwr use 
# Incident location, date and time etc 
# Subject information 
# Force used - rudimentry 
# Officer injurded


load("../401-2 Catagorical Regression Final Paper/Data/trr_actions.rds")

## Description: Details on the actions and responses by officers and subjects
# during the incident. Multiple rows may exist per TRR.

## Detailts 
# Data set on the action level (by officer and suspect)
# resistence type and level 
# action type - member and suspect
# force type


# Merge data so that there is one row for each type of force used
trr_full <- merge(trr_actions_data, trr_main_data, by = "trr_id")

# Create matching name for beat andc year 
trr_full$beat_id <- trr_full$beat_occ
trr_full$year <- trr_full$trr_year


##########################################################################
### Condencing TRR data to the icident level 
##########################################################################


###  ## Re-scaling the Force type codes 
# Each incident is spaced out across multiple rows but we want to collapse it into 
# one row for an incident level. To capture the variation in force this code creates a 
# new column for each level of force as a dichotomout outcome. A force level factor is 
# also craeted to give an ordered scale for force. Outcomes forv resistence level are also
# recorded

### Force types 

trr_full <- trr_full |>
  mutate(
    force.low = if_else(force_type %in% 
                          c("Verbal Commands",
                            "Member Presence",
                            "Physical Force - Holding"), 1L, 0L),
    force.intermediate = if_else(force_type %in% 
                                   c("Physical Force - Stunning",
                                     "Physical Force - Direct Mechanical",
                                     "Taser Display", "Other Force"), 1L, 0L),
    force.lesslethal = if_else(force_type %in% 
                                 c("Impact Weapon",
                                   "Chemical", 
                                   "Chemical (Authorized)", 
                                   "Taser"), 1L, 0L),
    force.lethal = if_else(force_type == "Firearm", 1L, 0L)
  )


### Resistance Types 

trr_full <- trr_full |>
  mutate(
    resistence.passive = if_else(resistance_level == "Passive", 1, 0),
    resistence.active = if_else(resistance_level == "Active", 1, 0),
    resistence.assult = if_else(resistance_level == "Assault/Battery", 1, 0),
    resistence.deadly = if_else(resistance_level == "Deadly Force", 1, 0),
    resistence = if_else(resistence.passive == 1, "Passive", "Active")
  )



## Collapsing 

trr_incident <- trr_full |>
  filter(person == "Member Action") |>
  group_by(trr_id) |>
  summarise(
    
    # Situation
    trr_id = first(trr_id),
    beat_id = first(beat_id),
    year = max(year, na.rm = TRUE),
    location = first(location),
    date = first(trr_date),
    
    # Subject
    subject_gender = first(subject_gender),
    subject_race = first(subject_race),
    subject_age = first(subject_age),
    subject_armed = first(subject_armed),
    subject_injured = first(subject_injured),
    
    officer_injured = first(officer_injured),
    officer_rank = first(rank),
    in_uniform = first(in_uniform),
    
    
    # Force indicators 
    force.low = max(force.low, na.rm = TRUE),
    force.intermediate = max(force.intermediate, na.rm = TRUE),
    force.lesslethal = max(force.lesslethal, na.rm = TRUE),
    force.lethal = max(force.lethal, na.rm = TRUE),
    
    # Resistence Indicators
    resistence.passive = max(resistence.passive, na.rm = TRUE),
    resistence.active = max(resistence.active, na.rm = TRUE),
    resistence.assult = max(resistence.assult, na.rm = TRUE),
    resistence.deadly = max(resistence.deadly, na.rm = TRUE)
  )


# Craete a level for force reflecting the highest level
trr_incident <- trr_incident |> 
  mutate(
    force.level = case_when(
      force.low == 1 & force.intermediate == 0 & force.lesslethal == 0 & force.lethal == 0 ~ 1,
      force.intermediate == 1 & force.lesslethal == 0 & force.lethal == 0 ~ 2,
      force.lesslethal == 1 & force.lethal == 0 ~ 3,
      force.lethal == 1 ~ 4
    ),
    resistence = case_when(
      resistence.passive == 1 & resistence.active == 0 & resistence.assult == 0 & resistence.deadly == 0 ~ "Passive",
      TRUE ~ "Active"
    )
  )


## Create a factor for force
trr_incident <- trr_incident |>
  mutate(
    force = factor(force.level,
                         levels = c("1", "2", "3", "4"),
                         labels = c("Low", "Intermediate", "Less Lethal", "Leathal"),
                         ordered = TRUE)
  )

### General Cleaning 

trr_incident <- trr_incident |> 
  mutate(
    subject_race = case_when(
      subject_race == "NATIVE AMERICAN/ALASKAN NATIVE" ~ "OTHER",
      subject_race == "ASIAN/PACIFIC ISLANDER" ~ "OTHER",
      subject_race == "" ~ "OTHER",
      TRUE ~ subject_race
      
    ) 
  )


# Reset ref for foactor

trr_incident$subject_race <- relevel(as.factor(trr_incident$subject_race), ref="WHITE")
trr_incident$resistence <- relevel(as.factor(trr_incident$resistence), ref="Passive")


##########################################################################
### Count Variables
##########################################################################


## Craete a count of incedents per block, per year

trr_counts <- trr_incident |>
  group_by(beat_id, year) |>
  summarise(force_count = n(), .groups = "drop")

trr_incident <- trr_incident |>
  left_join(trr_counts,
            by = c("beat_id", "year"),
            relationship = "many-to-one") 



trr_counts_low <- trr_incident |>
  filter(force.low == 1) |>
  group_by(beat_id, year) |>
  summarise(force_count_low = n(), .groups = "drop")

trr_incident <- trr_incident |>
  left_join(trr_counts_low,
            by = c("beat_id", "year"),
            relationship = "many-to-one") 



trr_count_int <- trr_incident |>
  filter(force.intermediate == 1) |>
  group_by(beat_id, year) |>
  summarise(force_count_int = n(), .groups = "drop")

trr_incident <- trr_incident |>
  left_join(trr_count_int,
            by = c("beat_id", "year"),
            relationship = "many-to-one") 

trr_count_ll <- trr_incident |>
  filter(force.lesslethal == 1) |>
  group_by(beat_id, year) |>
  summarise(force_count_ll = n(), .groups = "drop")

trr_incident <- trr_incident |>
  left_join(trr_count_ll,
            by = c("beat_id", "year"),
            relationship = "many-to-one") 

trr_count_le <- trr_incident |>
  filter(force.lethal == 1) |>
  group_by(beat_id, year) |>
  summarise(force_count_le = n(), .groups = "drop")

trr_incident <- trr_incident |>
  left_join(trr_count_le,
            by = c("beat_id", "year"),
            relationship = "many-to-one") 

trr_incident <- trr_incident |>
  mutate(
    force_count = ifelse(is.na(force_count), 0, force_count),
    force_count_low = ifelse(is.na(force_count_low), 0, force_count_low),
    force_count_int = ifelse(is.na(force_count_int), 0, force_count_int),
    force_count_ll = ifelse(is.na(force_count_ll), 0, force_count_ll),
    force_count_le = ifelse(is.na(force_count_le), 0, force_count_le),
  )



##########################################################################
### Joining the annual communityy survey 
##########################################################################

# Anual Community Suvey 
load("../401-2 Catagorical Regression Final Paper/Data/cpd_beats_acs.rds")

## Annual community survey data from the US census that has bean reacalabrated
# to correspond with beat areas


# Join census data to the ttr
trr_cens <- left_join(x = trr_incident, y = cpd_beats_acs, by = c("beat_id", "year"), relationship = "many-to-many")

## Remove rows without census data
trr <- trr_cens |>   filter(pct_white != "NA")

# This leaves us with the years 2012 to 2016. As other years do not map with the census data
# They have been dropped from the dataset at this point

##########################################################################
### Joining Chicago Level Crime Data 
##########################################################################

## Load crime data 
load("../401-2 Catagorical Regression Final Paper/Data/crime.rds")



### Calculating the rate of crimes per block

crimes <- crime_data |> 
  group_by(beat, year) |>
  summarise(crime_count = n()) |>
  mutate(beat_id = as.character(beat))



## Append total crimes to dataset
trr <- trr |>
  left_join(crimes, by = c("beat_id", "year"), relationship = "many-to-one")  |>
  mutate(
    t_pop = as.numeric(t_pop),
    count = as.numeric(crime_count),
    crime_rate = (count / t_pop) * 1000)


### Calculating the rate of homicides per block

homicide <- crime_data |> 
  filter(primary_type == "HOMICIDE") |>
  group_by(beat, year) |>
  summarise(homicide_count = n()) |>
  mutate(beat_id = as.character(beat))


## Append total crimes to dataset
trr <- trr |>
  left_join(homicide, by = c("beat_id", "year"), relationship = "many-to-one")  |>
  mutate(
    t_pop = as.numeric(t_pop),
    count = as.numeric(homicide_count),
    homicide_rate = (count / t_pop) * 1000,
    homicide_rate = ifelse(is.na(homicide_rate), 0, homicide_rate))
