# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")


# Params ------------------------------------------------------------------
bootstrap_n <- 1000
cores <- 10
delta <- 1/60

Period1_df<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-01-01", tz = "UTC") &
                                                      as.Date(Date) < as.Date("2003-12-01", tz = "UTC"))
period1_fit <- diff(log(Period1_df$Price))

estimates_w_mu <-  MLE_n_MoM_Analytic(delta_X = period1_fit, delta = delta, mu_incl = TRUE) 
estimates_wo_mu <-  MLE_n_MoM_Analytic(delta_X = period1_fit, delta = delta, mu_incl = FALSE) 

#   -----------------------------------------------------------------------

# boot_runs <- bootstrap_NIG_param(data_sample = period1_fit, bootstrap_n = bootstrap_n,
#                                  cores = cores, delta = delta, with_mu = TRUE)
# bootstrap_params_w_mu <- boot_runs$bootstrap_params_wo_mu %>% do.call(what = rbind)
# bootstrap_params_wo_mu <- boot_runs$bootstrap_params_w_mu %>% do.call(what = rbind)
# save(bootstrap_params_w_mu, file = "Code_Library/Validation/bootstrap_params_w_mu_2003.Rdata")
# save(bootstrap_params_wo_mu, file = "Code_Library/Validation/bootstrap_params_wo_mu_2003.Rdata")

load("Code_Library/Validation/bootstrap_params_w_mu_2003.Rdata")
load("Code_Library/Validation/bootstrap_params_wo_mu_2003.Rdata")


sd_w_mu <- bootstrap_sd(bootstrap_list = bootstrap_params_w_mu, n_bootstraps = 1000)

1.96*sd_w_mu[1] > estimates_w_mu$sigma
1.96*sd_w_mu[2] > estimates_w_mu$theta
1.96*sd_w_mu[3] > estimates_w_mu$kappa
1.96*sd_w_mu[4] > estimates_w_mu$mu



sd_wo_mu <- bootstrap_sd(bootstrap_list = bootstrap_params_wo_mu, n_bootstraps = 1000)

1.96*sd_wo_mu[1] > estimates_wo_mu$sigma
1.96*sd_wo_mu[2] > estimates_wo_mu$theta
1.96*sd_wo_mu[3] > estimates_wo_mu$kappa

#Option 1 limit
option1_limit <- SP500_2003_cleaned_n_synced %>% filter(isoweek(Date) %in% c(45:48))
option1_limit_fit <- diff(log(option1_limit$Price))


# NIG_limit_2003 <- bootstrap_NIG_param(data_sample = option1_limit_fit,
#                                       bootstrap_n = bootstrap_n, cores = cores, delta = delta, with_mu = FALSE)
# save(NIG_limit_2003, file = "Code_Library/Validation/bootstrap_limit_2003_NIG.Rdata")
load("Code_Library/Validation/bootstrap_limit_2003_NIG.Rdata")
bootstrap_sd(bootstrap_list = NIG_limit_2003, n_bootstraps = 1000)

# BS_full_2003 <- bootstrap_BS_param(data_sample = period1_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_full_2003, file = "Code_Library/Validation/bootstrap_2003_BS.Rdata")
load("Code_Library/Validation/bootstrap_2003_BS.Rdata")
bootstrap_sd(bootstrap_list = BS_full_2003, n_bootstraps = 1000)

# BS_limit_2003 <- bootstrap_BS_param(data_sample = option1_limit_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_limit_2003, file = "Code_Library/Validation/bootstrap_limit_2003_BS.Rdata")
load("Code_Library/Validation/bootstrap_limit_2003_BS.Rdata")
bootstrap_sd(bootstrap_list = BS_limit_2003, n_bootstraps = 1000)

# 2008  -----------------------------------------------------------------------

#Option 2
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

option2_full <- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                       as.Date(Date) < as.Date("2008-10-01", tz = "UTC")) 
option2_full_fit <- diff(log(option2_full$Price))


option2_limit <- option2_full %>% filter(isoweek(Date) %in% c(38:40))
option2_limit_fit <- diff(log(option2_limit$Price))

####
# NIG_2008_opt2 <- bootstrap_NIG_param(data_sample = option2_full_fit,
#                                       bootstrap_n = bootstrap_n, cores = cores, delta = delta, with_mu = FALSE)
# save(NIG_2008_opt2, file = "Code_Library/Validation/bootstrap_NIG_2008_opt2.Rdata")
load("Code_Library/Validation/bootstrap_NIG_2008_opt2.Rdata")
bootstrap_sd(bootstrap_list = NIG_2008_opt2, n_bootstraps = 1000)

# NIG_limit_2008_opt2 <- bootstrap_NIG_param(data_sample = option2_limit_fit,
#                                       bootstrap_n = bootstrap_n, cores = cores, delta = delta, with_mu = FALSE)
# save(NIG_limit_2008_opt2, file = "Code_Library/Validation/bootstrap_NIG_limit_2008_opt2.Rdata")
load("Code_Library/Validation/bootstrap_NIG_limit_2008_opt2.Rdata")
bootstrap_sd(bootstrap_list = NIG_limit_2008_opt2, n_bootstraps = 1000)

##
# BS_full_2008_opt2 <- bootstrap_BS_param(data_sample = option2_full_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_full_2008_opt2, file = "Code_Library/Validation/bootstrap_BS_full_2008_opt2.Rdata")
load("Code_Library/Validation/bootstrap_BS_full_2008_opt2.Rdata")
bootstrap_sd(bootstrap_list = BS_full_2008_opt2, n_bootstraps = 1000)

# BS_limit_2008_opt2 <- bootstrap_BS_param(data_sample = option2_limit_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_limit_2008_opt2, file = "Code_Library/Validation/bootstrap_BS_limit_2008_opt2.Rdata")
load("Code_Library/Validation/bootstrap_BS_limit_2008_opt2.Rdata")
bootstrap_sd(bootstrap_list = BS_limit_2008_opt2, n_bootstraps = 1000)
####

#Option 3
option3_full <- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                       as.Date(Date) < as.Date("2008-12-01", tz = "UTC")) 

option3_full_fit <- diff(log(option3_full$Price))

option3_limit <- option3_full %>% filter(isoweek(Date) %in% c(47))
option3_limit_fit <- diff(log(option3_limit$Price))

####
# NIG_2008_opt3 <- bootstrap_NIG_param(data_sample = option3_full_fit,
#                                      bootstrap_n = bootstrap_n, cores = cores, delta = delta, with_mu = FALSE)
# save(NIG_2008_opt3, file = "Code_Library/Validation/bootstrap_NIG_2008_opt3.Rdata")
load("Code_Library/Validation/bootstrap_NIG_2008_opt3.Rdata")
bootstrap_sd(bootstrap_list = NIG_2008_opt3, n_bootstraps = 1000)

# NIG_limit_2008_opt3 <- bootstrap_NIG_param(data_sample = option3_limit_fit,
#                                            bootstrap_n = bootstrap_n, cores = cores, delta = delta, with_mu = FALSE)
# save(NIG_limit_2008_opt3, file = "Code_Library/Validation/bootstrap_NIG_limit_2008_opt3.Rdata")
load("Code_Library/Validation/bootstrap_NIG_limit_2008_opt3.Rdata")
bootstrap_sd(bootstrap_list = NIG_limit_2008_opt3, n_bootstraps = 1000)

##
# BS_full_2008_opt3 <- bootstrap_BS_param(data_sample = option3_full_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_full_2008_opt3, file = "Code_Library/Validation/bootstrap_BS_full_2008_opt3.Rdata")
load("Code_Library/Validation/bootstrap_BS_full_2008_opt3.Rdata")
bootstrap_sd(bootstrap_list = BS_full_2008_opt3, n_bootstraps = 1000)

# BS_limit_2008_opt3 <- bootstrap_BS_param(data_sample = option3_limit_fit, bootstrap_n = bootstrap_n, delta = delta)
# save(BS_limit_2008_opt3, file = "Code_Library/Validation/bootstrap_BS_limit_2008_opt3.Rdata")
load("Code_Library/Validation/bootstrap_BS_limit_2008_opt3.Rdata")
bootstrap_sd(bootstrap_list = BS_limit_2008_opt3, n_bootstraps = 1000)
####




