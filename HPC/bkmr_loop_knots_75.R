##########################################################################################################################
## Loop BKMR knots through 100 seeds
## 51:75
## 11/18/2019
##########################################################################################################################

#install.packages("bkmr")
#install.packages("tidyverse")
## load required libraries
library(bkmr)
library(tidyverse)

##########################################################################################################################
## Data Manipulation 
##########################################################################################################################

## read in data and only consider complete data 
## this drops 327 individuals, but BKMR does not handle missing data
nhanes = na.omit(read_csv("Data/studypop.csv"))

## center/scale continous covariates and create indicators for categorical covariates
nhanes$age_z = scale(nhanes$age_cent) ## center and scale age
nhanes$agez_sq = nhanes$age_z^2 ## square this age variable
nhanes$bmicat2 = as.numeric(nhanes$bmi_cat3 == 2) ## 25 <= BMI < 30
nhanes$bmicat3 = as.numeric(nhanes$bmi_cat3 == 3) ## BMI >= 30 (BMI < 25 is the reference)
nhanes$educat1 = as.numeric(nhanes$edu_cat == 1) ## no high school diploma
nhanes$educat3 = as.numeric(nhanes$edu_cat == 3) ## some college or AA degree
nhanes$educat4 = as.numeric(nhanes$edu_cat == 4) ## college grad or above (reference is high schol grad/GED or equivalent)
nhanes$otherhispanic = as.numeric(nhanes$race_cat == 1) ## other Hispanic or other race - including multi-racial
nhanes$mexamerican = as.numeric(nhanes$race_cat == 2) ## Mexican American 
nhanes$black = as.numeric(nhanes$race_cat == 3) ## non-Hispanic Black (non-Hispanic White as reference group)
nhanes$wbcc_z = scale(nhanes$LBXWBCSI)
nhanes$lymphocytes_z = scale(nhanes$LBXLYPCT)
nhanes$monocytes_z = scale(nhanes$LBXMOPCT)
nhanes$neutrophils_z = scale(nhanes$LBXNEPCT)
nhanes$eosinophils_z = scale(nhanes$LBXEOPCT)
nhanes$basophils_z = scale(nhanes$LBXBAPCT)
nhanes$lncotinine_z = scale(nhanes$ln_lbxcot) ## to access smoking status, scaled ln cotinine levels

## our y variable - ln transformed and scaled mean telomere length
lnLTL_z = scale(log(nhanes$TELOMEAN)) 

## our Z matrix
mixture = with(nhanes, cbind(LBX074LA, LBX099LA, LBX118LA, LBX138LA, LBX153LA, LBX170LA, LBX180LA,
                             LBX187LA, LBX194LA, LBXHXCLA, LBXPCBLA, LBXD03LA, LBXD05LA, LBXD07LA,
                             LBXF03LA, LBXF04LA, LBXF05LA, LBXF08LA)) 

lnmixture = apply(mixture, 2, log)
lnmixture_z = scale(lnmixture)
colnames(lnmixture_z) = c(paste0("PCB",c(74, 99, 118, 138, 153, 170, 180, 187, 194, 169, 126)), 
                          paste0("Dioxin",1:3), paste0("Furan",1:4)) 

## our X matrix
covariates = with(nhanes, cbind(age_z, agez_sq, male, bmicat2, bmicat3, educat1, educat3, educat4, 
                                otherhispanic, mexamerican, black, wbcc_z, lymphocytes_z, monocytes_z, 
                                neutrophils_z, eosinophils_z, basophils_z, lncotinine_z)) 

############################################################################################################################
## Above code does not change
## Next begin looping over seeds
############################################################################################################################

### loop over knot seeds for Gaussian predictive process (to speed up BKMR with large datasets)

fit_knot <- function(seed) {
  set.seed(seed)
  knots100 <- fields::cover.design(lnmixture_z, nd = 100)$design
}

##########################################################################################################################
## Fit Model
##########################################################################################################################

### Group VS Fit with all Exposures using GPP and 100 Knots 
##### fit BKMR models WITH Gaussian predictive process using 100 knots

## Fit model over looped knots

fit_model <- function(knots) {
  set.seed(10)
  print(format(Sys.time(), "%c"))
  fit_bkmr_seed <-  kmbayes( y = lnLTL_z, Z = lnmixture_z, X = covariates, 
                             iter = 100000,
                             verbose = FALSE, varsel = TRUE,
                             groups = c(rep(1, times = 2), 2, rep(1, times = 6),
                                        rep(3, times = 2), rep(2, times = 7)), knots = knots)
}


#100 random seeds
repeat_knot_75 <- tibble(seed = 51:75) %>%
  mutate(knots = map(seed, ~fit_knot(seed = .x)))

repeat_knot_75 <- repeat_knot_75 %>%
  mutate(fits = map(knots, ~fit_model(knots = .x)))

##########################################################################################################################
## Posterior Inclusion Probabilities (PIPs)
##########################################################################################################################

get_pips <- function(model) {
  all_pips = ExtractPIPs(model)
  all_pips
}

repeat_knot_75 <- repeat_knot_75 %>%
  mutate(pips = map(fits, ~get_pips(model = .x)))

##########################################################################################################################
## Data Visualization
##########################################################################################################################

get_data_viz <- function(row) {
  ### change this for each model you fit and then rerun the code from here to the bottom
  modeltoplot      <- repeat_knot_75[row, 3][[1]][[1]]   ## name of model object
  modeltoplot.name <- c("fit ", row)                        ## name of model for saving purposes
  plot.name        <- c("fit ", row)                        ## part that changed in plot name 
  Z                <- lnmixture_z                           ## Z matrix to match what was used in model
  
  ### values to keep after burnin/thin
  sel <- seq(50001, 100000,by = 50)
  
  #### Univariable and Bivariable Exposure-Response Functions
  #### create dataframes for ggplot (this takes a little while to run)
  
  pred.resp.univar <- PredictorResponseUnivar(fit = modeltoplot, sel = sel, method = "approx")
  
  risks.overall <- OverallRiskSummaries(fit = modeltoplot, 
                                        qs = seq(0.25, 0.75, by = 0.05), 
                                        q.fixed = 0.5, method = "approx", sel = sel)
  
  list(pred.resp.univar = pred.resp.univar, 
       risks.overall = risks.overall)
}

repeat_knot_75 <- repeat_knot_75 %>% 
  mutate(plot_dat = map(1:25, ~get_data_viz(.x)))

repeat_knot_75 <- repeat_knot_75 %>% unnest_wider(plot_dat) %>% select(-fits)

save(repeat_knot_75, file = "bkmr_repeat_75_knots.RData")
