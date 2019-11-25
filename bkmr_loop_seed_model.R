##########################################################################################################################
## Loop BKMR model through 100 seeds
## 11/18/2019
##########################################################################################################################

#install.packages("here")
#install.packages("bkmr")
#install.packages("openssl")
## load required libraries
library(bkmr)
library(tidyverse)
library(openssl)

##########################################################################################################################
## Data Manipulation 
##########################################################################################################################

## read in data and only consider complete data 
## this drops 327 individuals, but BKMR does not handle missing data
nhanes = na.omit(read_csv(here::here("Data/studypop.csv")))

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

### create knots matrix for Gaussian predictive process (to speed up BKMR with large datasets)
#set.seed(1506744763) # use better seed now
set.seed(10)
knots100 <- fields::cover.design(lnmixture_z, nd = 100)$design

##########################################################################################################################
## Fit Model
##########################################################################################################################

### Group VS Fit with all Exposures using GPP and 100 Knots 
##### fit BKMR models WITH Gaussian predictive process using 100 knots

## Loop Over Seeds

fit_seed <- function(seed) {
  set.seed(seed)
  print(format(Sys.time(), "%c"))
  fit_bkmr_seed <-  kmbayes( y = lnLTL_z, Z = lnmixture_z, X = covariates, 
                             iter = 100000,
                             verbose = FALSE, varsel = TRUE,
                             groups = c(rep(1, times = 2), 2, rep(1, times = 6),
                                      rep(3, times = 2), rep(2, times = 7)), knots = knots100)
  }

## Generate actually random numbers (doubles) between 0 and 1.
## Multiply bt 2^31 to get randomly drawn seeds
# random_seeds <- as.integer(rand_num(100)*2^31)
# Do not rerun line above because you cannot set a seed for rand_num !!!
# 10 random seeds
random_seeds <- c(1696845011, 2146299107, 850084010, 239807331, 358435160, 1747805492, 1797926140, 398232577, 2086394305, 1230232207)

# 100 random seeds
# random_seeds <- c(1696845011, 2146299107, 850084010, 239807331, 358435160, 1747805492, 1797926140, 398232577, 2086394305, 1230232207, 
#                   1215510264, 1382974347, 1381764219, 340611980, 1122618262, 852928955, 862364730, 555833217, 103990313, 899651635, 
#                   683886414, 603752247, 2097606650,  396904838, 1542633574, 1097264073, 2124302833, 1299495204, 223424352, 
#                   1209358189, 1804605515, 807179409, 2102149603, 1769754379, 1866590524, 1323703037, 287739888, 1725306260, 
#                   2108205628, 982934702, 391377862, 474935663, 1506897475, 1839033940, 1973903620, 573755742, 1753707646, 
#                   233638062, 464472553, 524380691, 1125662733, 1360471694, 961860894, 1728557712, 2142456893, 285676556, 
#                   1552089477, 1062671500, 1352552202, 1101814281, 2107527674, 1812418288, 1046507888, 323191239, 290188361, 
#                   2030373549, 785869261, 476701173, 1489765601, 91004358, 147516962, 2129586546, 1458368111, 1156232023, 1073459776, 
#                   2008480909, 1354002038, 1041734203, 1218778759, 1002634823, 853751121, 1692817709, 1323847903, 99225906, 
#                   1235062094, 614952410, 833926949, 1885670789, 173294806, 35306487, 1742113516, 1439884190, 809936138, 
#                   748945584, 1420273796, 1598033662, 849625716, 1629716299, 1677728306, 1293766699)

repeat_model_seed <- tibble(seed = random_seeds) %>%
  mutate(fits = map(seed, ~fit_seed(seed = .x)))

repeat_model_seed

##########################################################################################################################
## Posterior Inclusion Probabilities (PIPs)
##########################################################################################################################

get_pips <- function(model) {
  all_pips = ExtractPIPs(model)
  all_pips
  }

repeat_model_seed <- repeat_model_seed %>%
  mutate(pips = map(fits, ~get_pips(model = .x)))

repeat_model_seed

##########################################################################################################################
## Data Visualization
##########################################################################################################################

get_data_viz <- function(row) {
  ### change this for each model you fit and then rerun the code from here to the bottom
  modeltoplot      <- repeat_model_seed[row, 2][[1]][[1]]   ## name of model object
  modeltoplot.name <- c("fit ", row)                        ## name of model for saving purposes
  plot.name        <- c("fit ", row)                        ## part that changed in plot name 
  Z                <- lnmixture_z                           ## Z matrix to match what was used in model

  ### values to keep after burnin/thin
  sel <- seq(50001, 100000,by = 50)

  ### access convergence with traceplots 
  # TracePlot(fit = modeltoplot, par = "beta", sel = sel)
  # TracePlot(fit = modeltoplot, par = "sigsq.eps", sel = sel)
  # 
  # TracePlot(fit = modeltoplot, par = "r", comp = 1, sel = sel)
  # TracePlot(fit = modeltoplot, par = "r", comp = 2, sel = sel)
  # TracePlot(fit = modeltoplot, par = "r", comp = 3, sel = sel)
  # TracePlot(fit = modeltoplot, par = "r", comp = 4, sel = sel)
  
  ### access convergence with traceplots 
  # TracePlot(fit = modeltoplot, par = "beta")
  # TracePlot(fit = modeltoplot, par = "sigsq.eps")
  # 
  # TracePlot(fit = modeltoplot, par = "r", comp = 1)
  # TracePlot(fit = modeltoplot, par = "r", comp = 2)
  # TracePlot(fit = modeltoplot, par = "r", comp = 3)
  # TracePlot(fit = modeltoplot, par = "r", comp = 4)

  #### Univariable and Bivariable Exposure-Response Functions
  #### create dataframes for ggplot (this takes a little while to run)

  pred.resp.univar <- PredictorResponseUnivar(fit = modeltoplot, sel = sel, method = "approx")
  
  #pred.resp.bivar  <- PredictorResponseBivar(fit = modeltoplot,  
                                          #min.plot.dist = 1, sel = sel, method = "approx")

  #pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = Z,
                                                      #both_pairs = TRUE, 
                                                      #qs = c(0.25, 0.5, 0.75))

  risks.overall <- OverallRiskSummaries(fit = modeltoplot, 
                                      qs = seq(0.25, 0.75, by = 0.05), 
                                      q.fixed = 0.5, method = "approx", sel = sel)

  #risks.singvar <- SingVarRiskSummaries(fit = modeltoplot, qs.diff = c(0.25, 0.75),
                                      #q.fixed = c(0.25, 0.50, 0.75), method = "approx")

  #risks.int <- SingVarIntSummaries(fit = modeltoplot, 
                                 #qs.diff = c(0.25, 0.75), qs.fixed = c(0.25, 0.75))
  
  list(pred.resp.univar = pred.resp.univar, 
       #pred.resp.bivar = pred.resp.bivar, 
       #pred.resp.bivar.levels = pred.resp.bivar.levels, 
       risks.overall = risks.overall)
       #risks.singvar = risks.singvar, 
       #risk.int = risks.int)
}

repeat_model_seed <- repeat_model_seed %>% 
  mutate(plot_dat = map(1:10, ~get_data_viz(.x)))

repeat_model_seed <- repeat_model_seed %>% unnest_wider(plot_dat) %>% select(-fits)

save(repeat_model_seed, file = "bkmr_repeat_10.RData")

load("bkmr_repeat_10.RData")

repeat_model_seed

##########################################################################################################################
## PIPs
##########################################################################################################################

repeat_model_seed %>% select(seed, pips) %>% unnest(cols = c(pips)) %>% 
  group_by(variable) %>% 
  summarize(group_mean = mean(groupPIP), group_sd = sd(groupPIP), ind_mean = mean(condPIP), ind_sd = sd(condPIP))

##########################################################################################################################
## Plots
##########################################################################################################################

##########################################################################################################################
## Univariate Exposure-Response Functions
##########################################################################################################################

repeat_model_seed %>% select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  mutate(variable = fct_recode(variable, "PCB 74" = "PCB74",
                               "PCB 99" = "PCB99",
                               "PCB 118" = "PCB118",
                               "PCB 138" = "PCB138",
                               "PCB 153" = "PCB153",
                               "PCB 170" = "PCB170",
                               "PCB 180" = "PCB180",
                               "PCB 187" = "PCB187",
                               "PCB 194" = "PCB194",
                               "1,2,3,6,7,8-hxcdd" = "Dioxin1",
                               "1,2,3,4,6,7,8-hpcdd" = "Dioxin2",
                               "1,2,3,4,6,7,8,9-ocdd" =  "Dioxin3",
                               "2,3,4,7,8-pncdf" =  "Furan1",
                               "1,2,3,4,7,8-hxcdf" =  "Furan2",
                               "1,2,3,6,7,8-hxcdf" =  "Furan3",
                               "1,2,3,4,6,7,8-hxcdf" =  "Furan4",
                               "PCB 169" =  "PCB169",
                               "PCB 126" = "PCB126")) %>%
  mutate(seed = as.character(seed)) %>% 
  ggplot(aes(z, est, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_smooth(aes(color = seed), stat = "identity") + 
  labs(title = "Univariate Exposure-Response Functions over 10 Model Seeds",
                                        y = "Estimate", x = "Exposure") +
  facet_wrap(~ variable) + theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill = "white"))

# ##########################################################################################################################
# #### Overall Mixture Effect
# ##########################################################################################################################

repeat_model_seed %>% select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>% 
  ggplot(aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_pointrange(aes(color = seed), size = .5) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Overall Mixture Effect over 10 Model Seeds",
       x = "Quantile", y = "Estimate")
  

