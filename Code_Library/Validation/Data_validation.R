# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")

# Params ------------------------------------------------------------------

delta <-  1/60

# 2008 figure -------------------------------------------------------------

load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

idx_1 <- which(SP500_2008_cleaned_n_synced$Date == as.POSIXct("2008-10-01 09:30:00", tz = "UTC")) - 1
idx_2 <- which(SP500_2008_cleaned_n_synced$Date == as.POSIXct("2008-10-17 16:00:00", tz = "UTC")) - 1
idx_3 <- which(SP500_2008_cleaned_n_synced$Date == as.POSIXct("2008-12-01 09:30:00", tz = "UTC")) - 1
idx_4 <- which(SP500_2008_cleaned_n_synced$Date == as.POSIXct("2008-12-19 16:00:00", tz = "UTC")) - 1

figure_2008 <- ggplot(data = SP500_2008_cleaned_n_synced,
                      mapping = aes(x = 0:(nrow(SP500_2008_cleaned_n_synced)-1), y = Price)) +
  geom_line() + ylab("S&P 500 index") + xlab("Datetime") + 
annotate("rect", xmin = idx_1, xmax = idx_2, ymin = min(SP500_2008_cleaned_n_synced$Price), ymax = Inf,  fill = "grey", alpha = 0.5) +
  annotate("rect", xmin = idx_3, xmax = idx_4, ymin = min(SP500_2008_cleaned_n_synced$Price), ymax = Inf, fill = "grey", alpha = 0.5) +
  annotate("text", x =c(floor((idx_1 + idx_2)/2), floor((idx_3 + idx_4)/2)),
           y = c(130, 130) , color = "red", label = c("Option 2", "Option3")) + 
  scale_x_continuous(breaks = c(0, (nrow(SP500_2008_cleaned_n_synced)-1),
                                idx_1, idx_2, idx_3, idx_4),
                     labels = c("2008-01-02", "2008-12-31", "2008-10-01",
                                "2008-10-17", "2008-12-01", "2008-12-19")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

png("sp500_2008.png", units = "px", width = 1600, height = 800, res = 300)
figure_2008
dev.off()


# Mu insignificant --------------------------------------------------------
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")

param_2003 <- param_week(SP500_2003_cleaned_n_synced, delta)

mu_2003 <- ggplot() + geom_line(aes(y = param_2003[,4], x = 1:nrow(param_2003))) + xlab("Weeks") + ylab(expression(hat(b)))
png("mu_2003.png", units = "px", width = 1600, height = 800, res = 300)
mu_2003
dev.off()
# Period 1 ----------------------------------------------------------------
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")
Period1_df<- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-01-01", tz = "UTC") &
                                                    as.Date(Date) < as.Date("2003-12-01", tz = "UTC"))

period1_fit <- diff(log(Period1_df$Price))

MLE_n_MoM_Analytic(delta_X = period1_fit, delta = delta, mu_incl = FALSE) 
#sigma = 0.004017535 , theta = 0.000117011, kappa = 0.1079041
esscher_h_special_case(sigma = 0.004017535, theta = 0.000117011, kappa = 0.1079041) #-7.749488
# BS_MLE(x_fit = period1_fit, delta = delta, n_runs = 100) #sigma =  0.00462  | mu =0.000128

Period1_figs <- validation_plots(nig_sigma =  0.004017535, nig_theta = 0.000117011,
                 nig_kappa = 0.1079041, nig_mu = 0, bs_sigma = 0.00462,
                 bs_mu = 0.000128, n = length(Period1_df$Price), delta = delta, T = length(Period1_df$Price) * delta,
                 Price_seq = Period1_df$Price)

period1_boxplot <- Period1_figs$boxplot
period1_qqplot <- Period1_figs$qqplot

set.seed(123)
NIG_prices <- Period1_df$Price[1]*exp(Sub_NIG_sim(T= length(Period1_df$Price) * delta,
                                           delta=delta, sigma=0.004017535,theta=0.000117011,kappa=0.1079041,
                                           mu_nig = 0))[-(length(Period1_df$Price)+1)]
set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Period1_df$Price[1], theta = c(0.000128 ,0.00462),
                          delta=delta, N=(length(Period1_df$Price)-1))

NIG_qq_plot_period1 <- QQ_plot_base(x = diff(log(Period1_df$Price)), y = diff(log(NIG_prices))) + ylab("NIG log returns")
BS_qq_plot_period1 <- QQ_plot_base(x = diff(log(Period1_df$Price)), y = diff(log(BS_prices))) + ylab("BS log returns")

qq_comb_1 <- gridExtra::grid.arrange(NIG_qq_plot_period1, BS_qq_plot_period1, ncol = 2)

ggsave("qq_comb_1.png", qq_comb_1, device = "png",  dpi = 250,
       width = 6, height = 3)

# Period 2 ----------------------------------------------------------------

load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")
period2_df <- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                            as.Date(Date) < as.Date("2008-10-01", tz = "UTC")) 

period2_fit <- diff(log(period2_df$Price))

#NIG fit
MLE_n_MoM_Analytic(delta_X = period2_fit, delta = delta, mu_incl = FALSE)
#sigma = 0.005550139 , theta = -0.0008033815, kappa = 2.645308, mu = 1.821492e-07
esscher_h_special_case(sigma = 0.005550139, theta = -0.0008033815, kappa = 1.831318) #25.58039
#BS fit
# BS_MLE(x_fit = period2_fit, delta = delta, n_runs = 100) #sigma = 0.00478, mu = -0.000120

#Validation figures
Period2_figs <- validation_plots(nig_sigma = 0.005550139, nig_theta = -0.0008033815,
                                 nig_kappa = 1.831318, nig_mu = 0, bs_sigma = 0.00478,
                                 bs_mu = -0.000120, n = length(period2_df$Price), delta = delta,
                                 T = length(period2_df$Price) * delta,
                                 Price_seq = period2_df$Price)
period2_boxplot <- Period2_figs$boxplot
period2_qqplot <- Period2_figs$qqplot

set.seed(123)
NIG_prices2 <- period2_df$Price[1]*exp(Sub_NIG_sim(T= length(period2_df$Price) * delta,
                                           delta=delta, sigma=0.005550139,theta=-0.0008033815,kappa=1.831318,
                                           mu_nig = 0))[-(length(period2_df$Price)+1)]
set.seed(123)
BS_prices2 <- sde::sde.sim(model = "BS" ,X0=period2_df$Price[1], theta = c(-0.000120 ,0.00478),
                          delta=delta, N=(length(period2_df$Price)-1))

NIG_qq_plot_period2 <- QQ_plot_base(x = diff(log(period2_df$Price)), y = diff(log(NIG_prices2))) + ylab("NIG log returns")
BS_qq_plot_period2 <- QQ_plot_base(x = diff(log(period2_df$Price)), y = diff(log(BS_prices2))) + ylab("BS log returns")

qq_comb_2 <- gridExtra::grid.arrange(NIG_qq_plot_period2, BS_qq_plot_period2, ncol = 2)

ggsave("qq_comb_2.png", qq_comb_2, device = "png",  dpi = 250,
       width = 6, height = 3)

# Period 3 ----------------------------------------------------------------

load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")
period3_df <- SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2008-01-01", tz = "UTC") &
                                                     as.Date(Date) < as.Date("2008-12-01", tz = "UTC")) 

period3_fit <- diff(log(period3_df$Price))

#NIG fit
MLE_n_MoM_Analytic(delta_X = period3_fit, delta = delta, mu_incl = FALSE)
#sigma = 0.008588097 , theta = -0.001156925, kappa = 2.922784
#esscher_h_special_case(sigma = 0.008588097, theta =  -0.001156925, kappa = 2.922784) #15.18597
#BS fit
# BS_MLE(x_fit = period3_fit, delta = delta, n_runs = 100) #sigma = 0.00725 , mu = -0.000198

#Validation figures
Period3_figs <- validation_plots(nig_sigma = 0.008588097 , nig_theta =  -0.001156925,
                                 nig_kappa = 2.922784, nig_mu = 0, bs_sigma = 0.00725,
                                 bs_mu = -0.000198, n = length(period3_df$Price), delta = delta,
                                 T = length(period3_df$Price) * delta,
                                 Price_seq = period3_df$Price)
period3_boxplot <- Period3_figs$boxplot
period3_qqplot <- Period3_figs$qqplot

set.seed(123)
NIG_prices3 <- period3_df$Price[1]*exp(Sub_NIG_sim(T= length(period3_df$Price) * delta,
                                            delta=delta, sigma=0.008588097,theta=-0.001156925,kappa=2.922784,
                                            mu_nig = 0))[-(length(period3_df$Price)+1)]
set.seed(123)
BS_prices3 <- sde::sde.sim(model = "BS" ,X0=period3_df$Price[1], theta = c(-0.000198 ,0.00725),
                           delta=delta, N=(length(period3_df$Price)-1))

NIG_qq_plot_period3 <- QQ_plot_base(x = diff(log(period3_df$Price)), y = diff(log(NIG_prices3))) + ylab("NIG log returns")
BS_qq_plot_period3 <- QQ_plot_base(x = diff(log(period3_df$Price)), y = diff(log(BS_prices3))) + ylab("BS log returns")

qq_comb_3 <- gridExtra::grid.arrange(NIG_qq_plot_period2, BS_qq_plot_period2, ncol = 2)

ggsave("qq_comb_3.png", qq_comb_3, device = "png",  dpi = 250,
       width = 6, height = 3)
# Combined validation figure ----------------------------------------------

ggsave("period1_boxplot.png", period1_boxplot, device = "png", dpi = 250,
       width = 6, height = 3)
ggsave("period1_qqplot.png", period1_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)

ggsave("period2_boxplot.png", period2_boxplot, device = "png",  dpi = 250,
       width = 6, height = 3)
ggsave("period2_qqplot.png", period2_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)

ggsave("period3_boxplot.png", period3_boxplot, device = "png",  dpi = 250,
       width = 6, height = 3)
ggsave("period3_qqplot.png", period3_qqplot, device = "png",  dpi = 250,
       width = 6, height = 3)
