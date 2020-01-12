
#v.1.0----
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")




delta <-  1/60
Period<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-01-01", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2003-12-01", tz = "UTC"))


period1_plot=param_month_plot(Period,delta,45:48)
period1_qqplot= period1_plot$qqplot
period1_boxplot= period1_plot$boxplot



Period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2008-10-01", tz = "UTC"))


period2_plot=param_month_plot(Period,delta,38:40)
period2_qqplot= period2_plot$qqplot
period2_boxplot= period2_plot$boxplot


Period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2008-12-01", tz = "UTC"))


period3_plot=param_month_plot(Period,delta,47)
period3_qqplot= period3_plot$qqplot
period3_boxplot= period3_plot$boxplot



ggsave("newperiod1_boxplot.png", period1_boxplot, device = "png", dpi = 250,
       width = 6, height = 3)
ggsave("newperiod1_qqplot.png", period1_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)

ggsave("newperiod2_boxplot.png", period2_boxplot, device = "png",  dpi = 250,
       width = 6, height = 3)
ggsave("newperiod2_qqplot.png", period2_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)

ggsave("newperiod3_boxplot.png", period3_boxplot, device = "png",  dpi = 250,
       width = 6, height = 3)
ggsave("newperiod3_qqplot.png", period3_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)

Period<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-11-03", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2003-12-01", tz = "UTC"))

Price_seq=Period$Price

set.seed(123)
NIG_prices <- Price_seq[1]*exp(Sub_NIG_sim(T= length(Price_seq) * delta,
                                           delta=delta, sigma=0.002669344,theta=1.249073e-06,kappa=0.0509927,
                                           mu_nig = 0))[-(length(Price_seq)+1)]
set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(4.585122e-06,0.00300184),
                          delta=delta, N=(length(Price_seq)-1))

new_NIG_qq_plot_period1 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(NIG_prices))) + ylab("NIG log returns")
new_BS_qq_plot_period1 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(BS_prices))) + ylab("BS log returns")

Period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-09-15", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2008-10-01", tz = "UTC"))

Price_seq=Period$Price

set.seed(123)
NIG_prices <- Price_seq[1]*exp(Sub_NIG_sim(T= length(Price_seq) * delta,
                                           delta=delta, sigma=0.01094479,theta=-0.0005876739,kappa=0.9950807,
                                           mu_nig = 0))[-(length(Price_seq)+1)]
set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(-0.0004373793,0.01009202),
                          delta=delta, N=(length(Price_seq)-1))

new_NIG_qq_plot_period2 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(NIG_prices))) + ylab("NIG log returns")
new_BS_qq_plot_period2 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(BS_prices))) + ylab("BS log returns")

Period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-11-17", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2008-11-24", tz = "UTC"))

Price_seq=Period$Price

set.seed(123)
NIG_prices <- Price_seq[1]*exp(Sub_NIG_sim(T= length(Price_seq) * delta,
                                           delta=delta, sigma=0.01628666,theta=-0.001662,kappa=0.1988205,
                                           mu_nig = 0))[-(length(Price_seq)+1)]
set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(-0.001607456,0.01438471),
                          delta=delta, N=(length(Price_seq)-1))

new_NIG_qq_plot_period3 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(NIG_prices))) + ylab("NIG log returns")
new_BS_qq_plot_period3 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(BS_prices))) + ylab("BS log returns")


new_qq_comb_1 <- gridExtra::grid.arrange(new_NIG_qq_plot_period1, new_BS_qq_plot_period1, ncol = 2)

ggsave("new_qq_comb_1.png", new_qq_comb_1, device = "png",  dpi = 250,
       width = 6, height = 3)

new_qq_comb_2 <- gridExtra::grid.arrange(new_NIG_qq_plot_period2, new_BS_qq_plot_period2, ncol = 2)

ggsave("new_qq_comb_2.png", new_qq_comb_2, device = "png",  dpi = 250,
       width = 6, height = 3)

new_qq_comb_3 <- gridExtra::grid.arrange(new_NIG_qq_plot_period3, new_BS_qq_plot_period3, ncol = 2)

ggsave("new_qq_comb_3.png", new_qq_comb_3, device = "png",  dpi = 250,
       width = 6, height = 3)



Period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-11-24", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2008-12-01", tz = "UTC"))

Price_seq=Period$Price

set.seed(123)
NIG_prices <- Price_seq[1]*exp(Sub_NIG_sim(T= length(Price_seq) * delta,
                                           delta=delta, sigma=0.01622207,theta=0.001441205,kappa=3588.634,
                                           mu_nig = 0))[-(length(Price_seq)+1)]
set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(0.001698259,0.008599516),
                          delta=delta, N=(length(Price_seq)-1))

new_NIG_qq_plot_period3 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(NIG_prices))) + ylab("NIG log returns")
new_BS_qq_plot_period3 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(BS_prices))) + ylab("BS log returns")

plot()

really_bad_qqplot= param_month_plot(Period,delta,48)$qqplot

ggsave("new_really_bad_qqplot.png", new_NIG_qq_plot_period3, device = "png",  dpi = 250,
       width = 6, height = 3)



# qqplot for test set --------------------------------------------------
rm(list = ls())

delta <-  1/60
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")
# 2003

train_period1<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-01-02", tz = "UTC") &
                                                         as.Date(Date) < as.Date("2003-11-28", tz = "UTC"))
Price_seq <- train_period1$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period1 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period1 <- BS_MLE(ret, delta = delta)

  
train_period2<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-11-03", tz = "UTC") &
                                                        as.Date(Date) < as.Date("2003-11-28", tz = "UTC"))

Price_seq <- train_period2$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period2 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period2 <- BS_MLE(ret, delta = delta)


test_period<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-12-01", tz = "UTC") &
                                                  as.Date(Date) < as.Date("2003-12-19", tz = "UTC"))

Price_seq <- test_period$Price
n <- length(Price_seq)
t <-  length(Price_seq) * delta

#Period 1
val_qqplot_p1 <- validation_plots(nig_sigma =  nig_fit_period1$sigma, nig_theta = nig_fit_period1$theta,
                 nig_kappa = nig_fit_period1$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                 bs_sigma = bs_fit_period1$sigma, bs_mu = bs_fit_period1$mu, 
                 Price_seq = Price_seq)

qqplot_p1 <- val_qqplot_p1$qqplot

# ggsave(filename = "qqplot03_testperiod_train1.png", plot = qqplot_p1, dpi = 200,
#       height = 2.3, width = 6, units = "in")

#period 2
val_qqplot_p2 <- validation_plots(nig_sigma =  nig_fit_period2$sigma, nig_theta = nig_fit_period2$theta,
                                  nig_kappa = nig_fit_period2$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                                  bs_sigma = bs_fit_period2$sigma, bs_mu = bs_fit_period2$mu, 
                                  Price_seq = Price_seq)

qqplot_p2 <- val_qqplot_p2$qqplot



# ggsave(filename = "qqplot03_testperiod_train2.png", plot = qqplot_p2, dpi = 200,
#       height = 2.3, width = 6, units = "in")


# 2008
# Before crash
train_period1<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-02", tz = "UTC") &
                                                         as.Date(Date) < as.Date("2008-09-30", tz = "UTC"))
Price_seq <- train_period1$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period1 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period1 <- BS_MLE(ret, delta = delta)


train_period2<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-09-15", tz = "UTC") &
                                                         as.Date(Date) < as.Date("2008-09-30", tz = "UTC"))

Price_seq <- train_period2$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period2 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period2 <- BS_MLE(ret, delta = delta)


test_period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-10-01", tz = "UTC") &
                                                       as.Date(Date) < as.Date("2008-10-17", tz = "UTC"))

Price_seq <- test_period$Price
n <- length(Price_seq)
t <-  length(Price_seq) * delta

#Period 1
val_qqplot_p1 <- validation_plots(nig_sigma =  nig_fit_period1$sigma, nig_theta = nig_fit_period1$theta,
                                  nig_kappa = nig_fit_period1$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                                  bs_sigma = bs_fit_period1$sigma, bs_mu = bs_fit_period1$mu, 
                                  Price_seq = Price_seq)

qqplot_p1 <- val_qqplot_p1$qqplot


# ggsave(filename = "duringqqplot08_testperiod_train1.png", plot = qqplot_p1, dpi = 200,
#       height = 2.3, width = 6, units = "in")

#period 2
val_qqplot_p2 <- validation_plots(nig_sigma =  nig_fit_period2$sigma, nig_theta = nig_fit_period2$theta,
                                  nig_kappa = nig_fit_period2$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                                  bs_sigma = bs_fit_period2$sigma, bs_mu = bs_fit_period2$mu, 
                                  Price_seq = Price_seq)

qqplot_p2 <- val_qqplot_p2$qqplot



# ggsave(filename = "duringqqplot08_testperiod_train2.png", plot = qqplot_p2, dpi = 200,
#       height = 2.3, width = 6, units = "in")



# After crash
train_period1<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-02", tz = "UTC") &
                                                         as.Date(Date) < as.Date("2008-11-30", tz = "UTC"))
Price_seq <- train_period1$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period1 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period1 <- BS_MLE(ret, delta = delta)


train_period2<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-11-17", tz = "UTC") &
                                                         as.Date(Date) < as.Date("2008-11-23", tz = "UTC"))

Price_seq <- train_period2$Price
ret <- diff(log(Price_seq))[-1]
n <- length(Price_seq)
t <-  length(Price_seq) * delta

nig_fit_period2 <- MLE_n_MoM_Analytic(ret, delta = delta, mu_incl = FALSE)

bs_fit_period2 <- BS_MLE(ret, delta = delta)


test_period<- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-12-01", tz = "UTC") &
                                                       as.Date(Date) < as.Date("2008-12-19", tz = "UTC"))

Price_seq <- test_period$Price
n <- length(Price_seq)
t <-  length(Price_seq) * delta

#Period 1
val_qqplot_p1 <- validation_plots(nig_sigma =  nig_fit_period1$sigma, nig_theta = nig_fit_period1$theta,
                                  nig_kappa = nig_fit_period1$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                                  bs_sigma = bs_fit_period1$sigma, bs_mu = bs_fit_period1$mu, 
                                  Price_seq = Price_seq)

qqplot_p1 <- val_qqplot_p1$qqplot

# ggsave(filename = "afterqqplot08_testperiod_train1.png", plot = qqplot_p1, dpi = 200,
#       height = 2.3, width = 6, units = "in")

#period 2
val_qqplot_p2 <- validation_plots(nig_sigma =  nig_fit_period2$sigma, nig_theta = nig_fit_period2$theta,
                                  nig_kappa = nig_fit_period2$kappa, delta = delta, n = n, T = t, nig_mu = 0,
                                  bs_sigma = bs_fit_period2$sigma, bs_mu = bs_fit_period2$mu, 
                                  Price_seq = Price_seq)

qqplot_p2 <- val_qqplot_p2$qqplot


# ggsave(filename = "afterqqplot08_testperiod_train2.png", plot = qqplot_p2, dpi = 200,
#       height = 2.3, width = 6, units = "in")



