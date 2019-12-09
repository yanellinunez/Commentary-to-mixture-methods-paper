##########################################################################################################################
## BKMR 100 Seed Figures
## 12/3/2019
## Lizzy
##########################################################################################################################

##########################################################################################################################
## 100 Knot Seeds
##########################################################################################################################

##########################################################################################################################
## PIPs
##########################################################################################################################

repeat_knot_seed %>% select(seed, pips) %>% unnest(cols = c(pips)) %>% 
  group_by(variable) %>% 
  summarize(group_mean = mean(groupPIP), group_sd = sd(groupPIP), ind_mean = mean(condPIP), ind_sd = sd(condPIP))

##########################################################################################################################
## Plots
##########################################################################################################################

##########################################################################################################################
## Univariate Exposure-Response Functions
##########################################################################################################################

repeat_knot_seed %>% select(seed, pred.resp.univar) %>% 
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
  labs(title = "Univariate Exposure-Response Functions over 10 Knot Seeds",
       y = "Estimate", x = "Exposure") +
  facet_wrap(~ variable) + theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill = "white"))

# ##########################################################################################################################
# #### Overall Mixture Effect
# ##########################################################################################################################

repeat_knot_seed %>% select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>% 
  ggplot(aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_pointrange(aes(color = seed), size = .5) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Overall Mixture Effect over 10 Knot Seeds",
       x = "Quantile", y = "Estimate")

##########################################################################################################################
## 100 MCMC Seeds
##########################################################################################################################

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


