##########################################################################
### 401-2 Catagirical Regression Methods | Analysis
##########################################################################


library(MASS)
library(marginaleffects)
library(modelsummary)
library(gofcat)
library(lmtest)
library(nnet) 
library(sjPlot)


##########################################################################
### Ordered Logistic Regression 
##########################################################################

ord.mod <-  polr(force ~ pct_af_am + pct_hisp + t_pop + pct_pov + resistence + subject_race + homicide_rate, Hess = T, data = trr)



ame <- avg_slopes(ord.mod)
modelsummary(ame, exponentiate = T,
             fmt = 2, shape = term:contrast ~ group, gof_map = c("nobs"), stars = TRUE,
             notes = "Exponetiated Coefficents",
             title = "Table 1: Average Marginal Effects - Ordinal Logit Model of Force Level")

ord.mod <-  polr(force ~ pct_af_am + pct_hisp + t_pop + pct_pov + subject_race + homicide_rate, Hess = T, data = trr)
## Proportional Odds
brant.test(ord.mod)



##########################################################################
### Multiple Logistic Regression 
##########################################################################


multi <- multinom(force ~ pct_af_am + pct_hisp + t_pop +  pct_pov + subject_race + homicide_rate, Hess = T, data = trr)
modelsummary(multi, 
             shape = model + term ~ response, 
             exponentiate = T, 
             coef_rename = c(
               "pct_af_am" = "% Black",
               "pct_hisp" = "% Hispanic",
               "t_pop" = "Total Population",
               "pct_pov" = "% In Poverty", 
               "homicide_rate" = "Homicide Rate"
               ),
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)

# Potentially re Add the resistance level of the suspect - this is good to account for as minorities may resist the 
# police more due to the tension (Holmes mentions this)


 
# Thoughts to come back to. total population appears to be important in some way however not in itself. 
# You need it in to make the other coefficients significant. Why?

#  Next Steps are two test whether total population reasons the likelihood rational   -- It doesnt?? 
#  Also lets create variables for the number of black and Hispanic residence and see what that does
#  You should create a racial heterogeneity mox


## Likihood ratio tests

df <- na.omit(trr[, c(
  "force", "pct_af_am", "pct_hisp", "pct_pov", "t_pop", "resistence", "subject_race", "homicide_rate")])

multi1 <- multinom(force ~ pct_af_am + pct_hisp + t_pop +  pct_pov + subject_race +  homicide_rate, Hess = T, data = df)
multi2 <- multinom(force ~ pct_af_am + pct_hisp + pct_pov + subject_race +  homicide_rate, Hess = T, data = df)

lrtest(multi1, multi2)


## Adding the total number of ethnic minorities 

df <- trr |>
  mutate(
    t_black = pct_af_am * t_pop,
    t_hisp = pct_hisp * t_pop
  )

multi <- multinom(force ~ t_black + t_hisp + pct_af_am + pct_hisp +  pct_pov + subject_race + homicide_rate, Hess = T, data = df)
modelsummary(multi,
             shape = model + term ~ response,
             exponentiate = T,
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)



### whites

multi <- multinom(force ~ pct_af_am + pct_hisp + pct_pov + t_pop + subject_race + resistence + homicide_rate, Hess = T, data = trr)
modelsummary(multi, 
             shape = model + term ~ response, 
             exponentiate = T, 
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)

multi <- multinom(force ~ pct_white + pct_af_am + pct_hisp+ pct_pov + t_pop + subject_race + resistence + homicide_rate, Hess = T, data = trr)

modelsummary(multi, 
             shape = model + term ~ response, 
             exponentiate = T, 
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)
            



## racial hetrogenity index 

# Convert percentages to proportions
trr$prop_white <- trr$pct_white / 100
trr$prop_black <- trr$pct_af_am / 100
trr$prop_hisp  <- trr$pct_hisp / 100
trr$prop_asian <- trr$pct_asian / 100
trr$prop_other <- trr$pct_other_nonhisp / 100


# Calculate Ethnic Fractionalization Index (EFI)
trr$racial_heterogeneity <- 1 - (
  trr$prop_white^2 +
    trr$prop_black^2 +
    trr$prop_hisp^2 +
    trr$prop_asian^2 +
    trr$prop_other^2
)

head(trr$racial_heterogeneity)


## Racial Minorotoiies 
multi <- multinom(force ~ pct_af_am + pct_hisp + racial_heterogeneity + pct_pov + t_pop + subject_race + homicide_rate, Hess = T, data = trr)
modelsummary(multi, 
             shape = model + term ~ response, 
             exponentiate = T, 
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)

## All racial
multi <- multinom(force ~ pct_white + pct_af_am + pct_hisp + racial_heterogeneity + pct_pov + t_pop + subject_race + homicide_rate, Hess = T, data = trr)
modelsummary(multi, 
             shape = model + term ~ response, 
             exponentiate = T, 
             fmt = "%.3f", notes = "Note: Exponentiated Coefficients",
             stars = T)


