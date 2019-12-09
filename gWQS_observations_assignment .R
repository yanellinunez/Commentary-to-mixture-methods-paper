result2 = gwqs(log_TELOMEAN ~ LBXWBCSI + LBXLYPCT + LBXMOPCT + LBXEOPCT + LBXBAPCT + LBXNEPCT + 
                 age_cent + age_sq + race_cat + bmi_cat3 + ln_lbxcot + edu_cat + male, 
               mix_name = mixture, data = dataset, q = 10, 
               validation = 0.6, valid_var = NULL, b = 100, b1_pos = TRUE, b1_constr = FALSE, 
               family = "gaussian", 
               seed = 123, wqs2 = FALSE, plots = TRUE, tables = TRUE)

dim(dataset)
dataset2 <- dataset
dataset2$valid.var <- c(sample(c(0,1), dim(dataset)[1], replace=TRUE))

gwqs_new = gwqs(log_TELOMEAN ~ wqs + LBXWBCSI + LBXLYPCT + LBXMOPCT + LBXEOPCT + LBXBAPCT + LBXNEPCT + 
                  age_cent + age_sq + race_cat + bmi_cat3 + ln_lbxcot + edu_cat + male, 
                mix_name = mixture, data = dataset2, q = 10, 
                validation = 0.6, valid_var = "valid.var", b = 100, b1_pos = TRUE, b1_constr = FALSE, 
                family = "gaussian", 
                seed = 123, plots = TRUE, tables = TRUE)


