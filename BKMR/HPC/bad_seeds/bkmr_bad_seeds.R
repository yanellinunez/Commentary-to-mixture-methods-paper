##########################################################################################################################
## Look at BKMR convergence on 4 seeds
## 11/18/2019
##########################################################################################################################

#install.packages("bkmr")
#install.packages("tidyverse")

## load required libraries
# Location for HPC!
library(bkmr)
library(tidyverse)

load("./BKMR/HPC/bad_seeds/bkmr_5_model_loop.RDA")
model_5 <- repeat_model_25 %>% dplyr::select(fits) %>% .[[1]] %>% .[[1]]

TracePlot(fit = model_5, par = "beta", comp = 1)
TracePlot(fit = model_5, par = "beta", comp = 2)
TracePlot(fit = model_5, par = "beta", comp = 3)
TracePlot(fit = model_5, par = "beta", comp = 4)
TracePlot(fit = model_5, par = "beta", comp = 5)
TracePlot(fit = model_5, par = "beta", comp = 6)
TracePlot(fit = model_5, par = "beta", comp = 7)
TracePlot(fit = model_5, par = "beta", comp = 8)
TracePlot(fit = model_5, par = "beta", comp = 9)
TracePlot(fit = model_5, par = "beta", comp = 10)
TracePlot(fit = model_5, par = "beta", comp = 11)
TracePlot(fit = model_5, par = "beta", comp = 12)
TracePlot(fit = model_5, par = "beta", comp = 13)
TracePlot(fit = model_5, par = "beta", comp = 14)
TracePlot(fit = model_5, par = "beta", comp = 15)
TracePlot(fit = model_5, par = "beta", comp = 16)
TracePlot(fit = model_5, par = "beta", comp = 17)
TracePlot(fit = model_5, par = "beta", comp = 18)
