# Source ------------------------------------------------------------------
source("Code_Library/functions.R")

# Estimation comparison -------------------------------------------------------------------

esti_compare <- function(T_grid, n){
T <- T_grid
delta <<- 1/36

x_fit <- Sub_NIG_sim(T = T, delta = delta, sigma = 0.0127, theta = 0.0013, kappa = 0.2873, mu_nig = 0.00003)
delta_X <<- diff(x_fit) %>% na.omit()

start_time <- Sys.time()
MLE_fit <- Optim_w_restarts_MLE(n = n,
                                delta_X =  delta_X, delta = delta) %>% 
                                  select(-opt_val) %>% mutate(run_time = Sys.time() - start_time,
                                                              method = "MLE")

start_time <- Sys.time()
MoM_fit <- Optim_w_restarts_MoM(n = n, delta_X =  delta_X, delta = delta) %>% 
            select(-opt_val) %>% mutate(run_time = Sys.time() - start_time,
                                        method = "MoM")

start_time <- Sys.time()
MoM_Analytic_fit <- MoM_Analytic(delta_X = delta_X, delta = delta) %>% 
mutate(run_time = Sys.time() - start_time,
       method = "MoM_a")

start_time <- Sys.time()
MLE_n_MoM_Analytic_fit <- MLE_n_MoM_Analytic(delta_X = delta_X, delta = delta) %>% 
         mutate(run_time = Sys.time() - start_time,
                method = "MLE_n_MoM_A")
return(
data.frame(rbind(MLE_fit, MoM_fit, MoM_Analytic_fit, MLE_n_MoM_Analytic_fit), T= T)
)
}

# 
# n_cores <- 4
# cl <- makeCluster(n_cores)
# 
# clusterExport(cl=cl, varlist=c(ls()), envir=environment())
# 
# esti_compare_result <- parLapply(cl, c(5, 15, 30, 100, 250, 500, 1000),
#                                   function(T_grid){
#                                     library(dplyr)
# 
#                                     esti_compare(T_grid = T_grid, n = 100)})
# stopCluster(cl)
# 
# esti_compare_result <- do.call(rbind, esti_compare_result)
# save(esti_compare_result, file = "Code_Library/estimation/estimations_compare_runs.Rdata" )

load("Code_Library/estimation/estimations_compare_runs.Rdata")

esti_compare_result <- esti_compare_result %>%
  mutate(error = abs(sigma - 0.0127) + abs(theta - 0.0013) + abs(kappa - 0.2873) + abs(mu - 0.00003))

ggplot(esti_compare_result, aes(x = T, y = error, color = method)) +
  geom_line(aes(group = method)) +
  geom_point() + 
  labs( x = "T", y = "Error", color = "Methods") + 
  scale_color_hue(labels = c("MLE", "MLE and MoM A", "MoM", "MoM A"))

ggplot(esti_compare_result %>% filter(method %in% c("MLE", "MLE_n_MoM_A")),
       aes(x = T, y = error, color = method)) +
  geom_line(aes(group = method)) +
  geom_point() + 
  labs( x = "T", y = "Error", color = "Methods") + 
  scale_color_hue(labels = c("MLE", "MLE and MoM A"))
