# 2003 --------------------------------------------------------------------
rm(list = ls())
source("Code_Library/functions.R")

load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")
Period1_df<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-01-01", tz = "UTC") &
                                                      as.Date(Date) < as.Date("2003-12-31", tz = "UTC"))
delta <-  1/60
# Weekly ------------------------------------------------------------------

parameter_week <- data.frame(sigma = 0, theta = 0, kappa = 0, mu = 0)

for (i in unique(isoweek(Period1_df$Date))[-1]) {
week <- Period1_df[isoweek(Period1_df$Date) == i, ]
fit_week <- diff(log(week$Price))
nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_week, delta = delta)

parameter_week <- rbind(parameter_week, nig_fit)

}

parameter_week <- parameter_week[-1, ]
rownames(parameter_week) <- unique(isoweek(Period1_df$Date))[-1]

xval <- unique(isoweek(Period1_df$Date))[-1]


gg_sig_week <- ggplot(parameter_week, aes(x = xval, 
                                          y = sigma, color = "sigma"))+
  ylab('')+
  xlab('')+
  geom_line()+
  scale_color_manual(labels = expression(hat(sigma)),
                     breaks = c("sigma"),
                     values = c("Blue"),
                     name = "")

gg_mu_week <- ggplot(parameter_week, aes(x = xval, y = mu, color = "mu"))+
  geom_line()+
  xlab('')+
  ylab('')+
  scale_color_manual(labels = expression(hat('b')),
                     breaks = c("mu"),
                     values = c("Blue"),
                     name = "")


gg_theta_week <- ggplot(parameter_week, aes(x = xval, 
                                            y = theta, color = "theta"))+
  geom_line()+
  xlab('')+
  ylab('')+
  scale_color_manual(labels = expression(hat(theta)),
                     breaks = c("theta"),
                     values = c("Blue"),
                     name = "")

gg_kappa_week <- ggplot(parameter_week, aes(x = xval, 
                                            y = kappa, color = "kappa"))+
  geom_line()+
  ylab('')+
  xlab('')+
  scale_color_manual(labels = expression(hat(kappa)),
                     breaks = c("kappa"),
                     values = c("Blue"),
                     name = "")


weeks_2003 <- grid.arrange(gg_sig_week, gg_mu_week, gg_theta_week, gg_kappa_week, nrow = 2)

 # ggsave(filename = "weeks_03.png", plot = weeks_2003, dpi = 200,
 #       height = 2.3, width = 6, units = "in")


 # 
 # Monthly -----------------------------------------------------------------
parameter_month <- data.frame(sigma = 0, theta = 0, kappa = 0, mu = 0)

for (i in unique(month(Period1_df$Date))) {
  mdr <- Period1_df[ month(Period1_df$Date) == i, ]
  fit_mdr <- diff(log(mdr$Price))
  nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_mdr, delta = delta)
  
  parameter_month <- rbind(parameter_month, nig_fit)
  
}

parameter_month <- parameter_month[-1, ]
rownames(parameter_month) <- unique(month(Period1_df$Date))

xval <- unique(month(Period1_df$Date))

gg_sig_month <- ggplot(parameter_month, aes(x = xval, 
                                            y = sigma, color = "sigma"))+
  ylab('')+
  xlab('')+
  geom_line()+
  scale_color_manual(labels = expression(hat(sigma)),
                     breaks = c("sigma"),
                     values = c("red"),
                     name = "")

gg_mu_month <- ggplot(parameter_month, aes(x = xval, 
                                           y = mu, color = "mu"))+
  geom_line()+
  ylab('')+
  xlab('')+
  scale_color_manual(labels = expression(hat('b')),
                     breaks = c("mu"),
                     values = c("red"),
                     name = "")


gg_theta_month <- ggplot(parameter_month, aes(x = xval, 
                                              y = theta, color = "theta"))+
  geom_line()+
  # coord_cartesian(ylim = c(min(parameter_month$theta[-5]),max(parameter_month$theta[-5])))+
  ylab('')+
  xlab('')+
  # annotate("text", x = 5,
  #          y = -0.00065 , color = "black", label = -0.86, size = 2) + 
  scale_color_manual(labels = expression(hat(theta)),
                     breaks = c("theta"),
                     values = c("red"),
                     name = "")

gg_kappa_month<- ggplot(parameter_month, aes(x = xval, 
                                             y = kappa, color = "kappa"))+
  geom_line()+
  # coord_cartesian(ylim = c(min(parameter_month$kappa),max(parameter_month$kappa)))+
  ylab('')+
  xlab('')+
  # annotate("text", x = 5,
  #          y = 9 , color = "black", label = 167.45, size = 2) + 
  scale_color_manual(labels = expression(hat(kappa)),
                     breaks = c("kappa"),
                     values = c("red"),
                     name = "")


month_2003 <- grid.arrange(gg_sig_month, gg_mu_month, gg_theta_month, gg_kappa_month, nrow = 2)

# ggsave(filename = "monthly_03.png", plot = month_2003, dpi = 200,
#       height = 2.3, width = 6, units = "in")




# 2008 --------------------------------------------------------------------
rm(list = ls())
source("Code_Library/functions.R")

load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")
Period1_df<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                      as.Date(Date) < as.Date("2008-12-31", tz = "UTC"))
delta <-  1/60

# Weekly ------------------------------------------------------------------

parameter_week <- data.frame(sigma = 0, theta = 0, kappa = 0, mu = 0)

for (i in unique(isoweek(Period1_df$Date))) {
  # week <- Period1_df[isoweek(Period1_df$Date) == i, ]
  # fit_week <- diff(log(week$Price))
  # # nig_fit <- Optim_w_restarts_MLE(n = 100, delta_X = fit_week, delta = delta)
  # 
    nig_fit=tryCatch({
    week <- Period1_df[ isoweek(Period1_df$Date) == i, ]
    fit_week <- diff(log(week$Price))
    nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_week, delta = delta)
  }, error=function(e) {
    week <- Period1_df[ isoweek(Period1_df$Date) == i, ]
    fit_week <- diff(log(week$Price))
    nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_week, delta = delta)[-5]
  })
  
  
  
  parameter_week <- rbind(parameter_week, nig_fit)
print(i)
  }

parameter_week <- parameter_week[-1, ]
rownames(parameter_week) <- unique(isoweek(Period1_df$Date))

xval <- unique(isoweek(Period1_df$Date))

gg_sig_week <- ggplot(parameter_week, aes(x = xval, 
                                            y = sigma, color = "sigma"))+
  ylab('')+
  xlab('')+
  geom_line()+
  scale_color_manual(labels = expression(hat(sigma)),
                     breaks = c("sigma"),
                     values = c("Blue"),
                     name = "")

gg_mu_week <- ggplot(parameter_week, aes(x = xval, 
                                          y = mu, color = "mu"))+
  geom_line()+
  xlab('')+
  ylab('')+
  scale_color_manual(labels = expression(hat('b')),
                     breaks = c("mu"),
                     values = c("Blue"),
                     name = "")


gg_theta_week <- ggplot(parameter_week, aes(x = xval, 
                                         y = theta, color = "theta"))+
  geom_line()+
  xlab('')+
  ylab('')+
  scale_color_manual(labels = expression(hat(theta)),
                     breaks = c("theta"),
                     values = c("Blue"),
                     name = "")
  
gg_kappa_week <- ggplot(parameter_week, aes(x = xval, 
                                            y = kappa, color = "kappa"))+
  geom_line()+
  ylab('')+
  xlab('')+
  scale_color_manual(labels = expression(hat(kappa)),
                     breaks = c("kappa"),
                     values = c("Blue"),
                     name = "")


weeks_2008 <- grid.arrange(gg_sig_week, gg_mu_week, gg_theta_week, gg_kappa_week, nrow = 2)

 # ggsave(filename = "weekly_08.png", plot = weeks_2008, dpi = 200,
 #       height = 2.3, width = 6, units = "in")


# Monthly -----------------------------------------------------------------
parameter_month <- data.frame(sigma = 0, theta = 0, kappa = 0, mu = 0)
for (i in unique(month(Period1_df$Date))) {
  # week <- Period1_df[isoweek(Period1_df$Date) == i, ]
  # fit_week <- diff(log(week$Price))
  # # nig_fit <- Optim_w_restarts_MLE(n = 100, delta_X = fit_week, delta = delta)
  # 
  nig_fit=tryCatch({
    week <- Period1_df[ month(Period1_df$Date) == i, ]
    fit_week <- diff(log(week$Price))
    nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_week, delta = delta)
  }, error=function(e) {
    week <- Period1_df[ month(Period1_df$Date) == i, ]
    fit_week <- diff(log(week$Price))
    nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_week, delta = delta)[-5]
  })
  
  parameter_month<- rbind(parameter_month, nig_fit)
  print(i)
}

parameter_month <- parameter_month[-1, ]
rownames(parameter_month) <- unique(month(Period1_df$Date))


xval <- unique(month(Period1_df$Date))

gg_sig_month <- ggplot(parameter_month, aes(x = xval, 
                                          y = sigma, color = "sigma"))+
  ylab('')+
  xlab('')+
  geom_line()+
  scale_color_manual(labels = expression(hat(sigma)),
                     breaks = c("sigma"),
                     values = c("red"),
                     name = "")
# expression(paste("Value is ", sigma,",", R^{2},'=0.6'))
gg_mu_month <- ggplot(parameter_month, aes(x = xval, 
                                         y = mu, color = "mu"))+
  geom_line()+
  ylab('')+
  xlab('')+
  scale_color_manual(labels = c(expression(hat('b'))),
                     breaks = c("mu"),
                     values = c("red"),
                     name = "")


gg_theta_month <- ggplot(parameter_month, aes(x = xval, 
                                            y = theta, color = "theta"))+
  geom_line()+
  coord_cartesian(ylim = c(min(parameter_month$theta[-5]),max(parameter_month$theta[-5])))+
  ylab('')+
  xlab('')+
  # annotate("text", x = 5,
  #          y = -0.00065 , color = "black", label = -0.86, size = 2) + 
  scale_color_manual(labels = c(expression(hat(theta))),
                     breaks = c("theta"),
                     values = c("red"),
                     name = "")

gg_kappa_month<- ggplot(parameter_month, aes(x = xval, 
                                            y = kappa, color = "kappa"))+
  geom_line()+
  coord_cartesian(ylim = c(min(parameter_month$kappa[-5]),max(parameter_month$kappa[-5])))+
  ylab('')+
  xlab('')+
  # annotate("text", x = 5,
  #          y = 9 , color = "black", label = 167.45, size = 2) + 
  scale_color_manual(labels = c(expression(hat(kappa))),
                     breaks = c("kappa"),
                     values = c("red"),
                     name = "")


month_2008 <- grid.arrange(gg_sig_month, gg_mu_month, gg_theta_month, gg_kappa_month, nrow = 2)

# ggsave(filename = "monthly_08.png", plot = month_2008, dpi = 200,
#       height = 2.3, width = 6, units = "in")

