##########################################################################################################################
## BKMR 100 Seed Figures
## 12/3/2019
## Lizzy
##########################################################################################################################

library(tidyverse)

##########################################################################################################################
## 100 Knot Seeds
##########################################################################################################################

# knots_out <- tibble()
# 
# for (i in 1:100) {
#   load(paste0("./HPC/HPC_out/bkmr_", i, "_knots_loop.RDA"))
#   knots_out[i,1:4] <- repeat_knot_25
# }

#save(knots_out, file = "./HPC/knots_output.RDA")
load("./BKMR/HPC/knots_output.RDA")
#knots_out <- knots_out %>% unnest_wider(plot_dat)

identical(knots_out$knots[[1]], knots_out$knots[[3]])
identical(knots_out$pips[[1]], knots_out$pips[[3]])
identical(knots_out$pred.resp.univar[[1]], knots_out$pred.resp.univar[[25]])
identical(knots_out$risks.overall[[1]], knots_out$risks.overall[[37]])
knots_out

##########################################################################################################################
## PIPs
##########################################################################################################################

knots_out %>% 
  dplyr::select(seed, pips) %>% unnest(cols = c(pips)) %>% 
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
  group_by(variable) %>% 
  summarise(group_min = min(groupPIP), group_med = median(groupPIP), 
            group_max = max(groupPIP)
            , ind_min = min(condPIP),    ind_med = median(condPIP),    ind_max = max(condPIP)) %>% 
  mutate_if(is.double, round, 4) %>%
  mutate(diff = (ind_max - ind_med) / ind_med,
         range = (ind_max - ind_min))

##########################################################################################################################
## Plots
##########################################################################################################################

##########################################################################################################################
## Univariate Exposure-Response Functions
##########################################################################################################################
# Median
mediank <- knots_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  mutate(seed = as.character(seed)) %>%
  group_by(z, variable) %>% 
  summarise(mediann = median(est),
            range = max(est) - min(est),
            sderr = sd(est))

# All seeds
knots_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  left_join(., mediank, by = c("z", "variable")) %>% 
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
  #filter(variable == "2,3,4,7,8-pncdf" | variable == "PCB 74") %>% 
  ggplot(aes(z, est, group = seed, ymin = est - 1.96*se, ymax = est + 1.96*se)) +
  geom_hline(yintercept = 00, linetype = "dashed") +
  geom_smooth(aes(group = seed), size = 0.25, stat = "identity", alpha = 0.1) + 
  #geom_smooth(aes(x = z, y = mediann, 
  #                ymin = mediann - 3*sderr, ymax = mediann + 3*sderr), size = 0.5, # med +- sd error, wrong...
  #            fill = "lightskyblue", alpha = 0.1, stat = "identity") + 
  facet_wrap(~ variable) +
  labs(y = "Estimate", x = "Exposure") +
  theme_minimal() +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"))

knots_out %>% 
  dplyr::select(seed, pred.resp.univar) %>% 
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
  #filter(variable == "1,2,3,4,6,7,8-hxcdf") %>% 
  ggplot(aes(z, est, ymin = est - se/2, ymax = est + se/2, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_smooth(aes(color = seed), stat = "identity") + 
  labs(title = "Univariate Exposure-Response Functions over 100 Knot Seeds",
       y = "Estimate", x = "Exposure") +
  facet_wrap(~ variable) + theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_rect(fill = "white"))

# ##########################################################################################################################
# #### Overall Mixture Effect
# ##########################################################################################################################

knots_out %>% select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>% 
  ggplot(aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_pointrange(aes(color = seed), size = .5) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Overall Mixture Effect over 100 Knot Seeds",
       x = "Quantile", y = "Estimate")

knots_out %>% select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>%
  group_by(seed) %>% 
  filter(any(est < -0.025)) %>% 
  pivot_wider(id_cols = c(seed), names_from = quantile, values_from = c(est, sd))

# 100 / 100 have overall effects

##########################################################################################################################
## 100 MCMC Seeds
##########################################################################################################################

# model_out <- tibble()
# 
# for (i in 1:100) {
#   load(paste0("./HPC/HPC_out/bkmr_", i, "_model_loop.RDA"))
#   model_out[i,1:3] <- repeat_model_25
# }


#save(model_out, file = "./HPC/model_output.RDA")
load("./BKMR/HPC/model_output.RDA")
model_out <- model_out %>% unnest_wider(plot_dat)
model_out

##########################################################################################################################
## PIPs
##########################################################################################################################

model_out %>% 
  dplyr::select(seed, pips) %>% unnest(cols = c(pips)) %>% 
  group_by(variable) %>% 
  summarise(group_mean = mean(groupPIP), group_sd = sd(groupPIP), ind_mean = mean(condPIP), ind_sd = sd(condPIP))

model_out %>% 
  dplyr::select(seed, pips) %>% unnest(cols = c(pips)) %>% 
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
  group_by(variable) %>% 
  summarise(group_min = min(groupPIP), group_med = median(groupPIP), group_max = max(groupPIP), 
            ind_min = min(condPIP),    ind_med = median(condPIP),    ind_max = max(condPIP)) %>% 
  mutate(diff = (ind_max - ind_med) / ind_med,
         range = (ind_max - ind_min))

##########################################################################################################################
## Plots
##########################################################################################################################

##########################################################################################################################
## Univariate Exposure-Response Functions
##########################################################################################################################

# Median
mediann <- model_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  mutate(seed = as.character(seed)) %>%
  group_by(z, variable) %>% 
  summarise(mediann = median(est),
            range = max(est) - min(est),
            sderr = sd(est),
            q25 = quantile(est, probs = 0.25),
            q75 = quantile(est, probs = 0.75))

# All seeds
model_plot <- model_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  left_join(., mediann, by = c("z", "variable")) %>% 
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
  #filter(variable == "2,3,4,7,8-pncdf" | variable == "PCB 74") %>% 
  ggplot(aes(z, est, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed") +
  geom_smooth(aes(group = seed,
                  #ymin = est - 1.96*se,
                  #ymax = est + 1.96*se
                  ), alpha = 0.01, size = 0.25,
              color = "blue", stat = "identity") + 
  #geom_smooth(aes(x = z, y = mediann, 
  #                ymin = q25, ymax = q75), size = 0.5, # med +- iqr
  #                fill = "lightskyblue", alpha = 0.1, stat = "identity") + 
  facet_wrap(~ variable) +
  labs(y = "Estimate", x = "Exposure") +
  theme_minimal() +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"))

pdf("./Figures/BKMR_model_plot.pdf")
model_plot
dev.off()

# For paper
pdf("./Figures/BKMR_2pop_plot.pdf")
model_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  left_join(., mediann, by = c("z", "variable")) %>% 
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
  filter(variable == "2,3,4,7,8-pncdf" | variable == "PCB 74") %>% 
  ggplot(aes(z, est, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed") +
  geom_smooth(aes(group = seed,
                  #ymin = est - 1.96*se,
                  #ymax = est + 1.96*se
  ), alpha = 0.01, size = 0.25,
  color = "blue", stat = "identity") + 
  #geom_smooth(aes(x = z, y = mediann, 
  #                ymin = q25, ymax = q75), size = 0.5, # med +- iqr
  #                fill = "lightskyblue", alpha = 0.1, stat = "identity") + 
  facet_wrap(~ variable) +
  labs(y = "Estimate", x = "Exposure") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"))
dev.off()

model_out %>% dplyr::select(seed, pred.resp.univar) %>% 
  unnest(cols = c(pred.resp.univar)) %>%
  left_join(., mediann, by = c("z", "variable")) %>% 
  mutate(seed = as.character(seed)) %>%
  filter(variable %in% c("PCB99", "PCB118", "PCB126", "PCB180", "PCB169", "Furan1")) %>% 
  group_by(seed, variable) %>% 
  #filter(any(z < -1) & any(est < -0.05)) %>% 
  ggplot(aes(z, est, group = seed,
             ymin = est - 1.96*se,
             ymax = est + 1.96*se
             )) +
  geom_hline(yintercept = 00, linetype = "dashed") +
  geom_smooth(aes(group = seed), alpha = 0.01, size = 0.25, stat = "identity") + 
  #geom_smooth(aes(x = z, y = mediann, 
  #                ymin = q25, ymax = q75), size = 0.5, # med +- iqr
  #                fill = "lightskyblue", alpha = 0.1, stat = "identity") + 
  labs(y = "Estimate", x = "Exposure") +
  facet_wrap(~variable)
  theme_minimal()
  
# ##########################################################################################################################
# #### Overall Mixture Effect
# ##########################################################################################################################

model_out %>% 
  dplyr::select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>% 
  group_by(seed) %>% 
  filter(any(quantile < 0.3) & !any(est < -0.1)) %>% 
  ggplot(aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, group = seed)) +
  geom_hline(yintercept = 00, linetype = "dashed", color = "red") +
  geom_pointrange(aes(color = seed), size = .5) + 
  theme_bw() +
  #theme(legend.position = "none") +
  labs(title = "Overall Mixture Effect over 100 Model Seeds",
       x = "Quantile", y = "Estimate")

model_out %>% 
  dplyr::select(seed, risks.overall) %>% 
  mutate(seed = as.character(seed)) %>% 
  unnest(cols = c(risks.overall)) %>%
  group_by(seed) %>% 
  filter(any(quantile == 0.25 & est < -0.025)) %>% 
  pivot_wider(id_cols = c(seed), names_from = quantile, values_from = c(est, sd))

# 96 / 100 have overall effect