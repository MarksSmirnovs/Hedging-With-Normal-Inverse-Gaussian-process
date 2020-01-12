# 2003  --------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")

Price_seq <- SP500_2003_cleaned_n_synced %>% filter(as.Date(Date) >= as.Date("2003-12-01"),
                                                    as.Date(Date) < as.Date("2003-12-20")) %>% 
  .$Price
Datetime_seq <- SP500_2003_cleaned_n_synced %>% filter(as.Date(Date) >= as.Date("2003-12-01"),
                                                       as.Date(Date) < as.Date("2003-12-20")) %>% 
  .$Date

delta <- 1/60
expir_T <-  length(Price_seq) * delta
K <- 106.51
S_0 <- 106.51
time_grid <- seq(0, expir_T, delta)[-1]
final_payoff <- max(tail(Price_seq,1) - K, 0)

## 2003 full
mean_var_2003 <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa = 0.1079041, sigma = 0.004017535, theta = 0.000117011, time_grid = time_grid)
mean_var_2003$H[5864:5865,1] <- NA; mean_var_2003$phi[,1][1] <- 0 #Unstable 

## 2003 limit
mean_var_2003_limit <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa = 0.0509927, sigma = 0.002669344, theta = 1.249073e-06, time_grid = time_grid)
mean_var_2003_limit$H[5864:5865,1] <- NA; mean_var_2003_limit$phi[,1][1] <- 0 #Unstable 

#Call prices


call_prices <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_2003$H[,1], fill = "Mean-Var"), color = "Blue") +
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_2003_limit$H[,1], fill = "Mean-Varlimit"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(Price_seq)-1), xmax = 1:(length(Price_seq)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("Mean-Var limit", "Mean-Var", "S(t) - K"),
                    breaks = c("Mean-Varlimit", "Mean-Var", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))


#Minute
hedge_1min <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_2003$phi[,1], mean_var_2003$H[1,1])

#10 Minute
Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

hedge_10min <- mean_var_2003$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min, mean_var_2003_limit$H[1,1])
hedge_10min <- apply(hedge_10min$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour
Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

hedge_hour <- mean_var_2003$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour,  mean_var_2003_limit$H[1,1])
hedge_hour <- apply(hedge_hour$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min[1:5865], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour[1:5865], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min,1) - final_payoff)
abs(tail(hedge_hour,1) - final_payoff)


#Limit

#Minute
hedge_1min_limit <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_2003_limit$phi[,1], mean_var_2003_limit$H[1,1])

#10 Minute

hedge_10min_limit <- mean_var_2003_limit$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min_limit <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min_limit, mean_var_2003_limit$H[1,1])
hedge_10min_limit <- apply(hedge_10min_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour

hedge_hour_limit <- mean_var_2003_limit$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour_limit <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour_limit,  mean_var_2003_limit$H[1,1])
hedge_hour_limit <- apply(hedge_hour_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot_limit <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min_limit[1:5865], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min_limit$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour_limit[1:5865], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min_limit$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min_limit,1) - final_payoff)
abs(tail(hedge_hour_limit,1) - final_payoff)

ggsave(filename = "mean_var_hedge_plot_2003.png", plot = mean_var_hedge_plot, dpi = 200,
     height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_hedge_plot_2003_limit.png", plot = mean_var_hedge_plot_limit, dpi = 200,
     height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_call_2003.png", plot = call_prices, dpi = 200,
     height = 2.3, width = 6, units = "in")

# 2008 before crash  --------------------------------------------------------------
rm(list = ls())
source("Code_Library/functions.R")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

Price_seq <- subset(SP500_2008_cleaned_n_synced, as.Date(Date, tz = "UTC") >= as.Date("2008-10-01", tz = "UTC") &
                      as.Date(Date, tz = "UTC") < as.Date("2008-10-18", tz = "UTC"))

Datetime_seq <- Price_seq$Date
Price_seq <- Price_seq$Price
delta <- 1/60
expir_T <-  length(Price_seq) * delta
T <-  length(Price_seq) * delta
K <- 115.94
S_0 <- 115.94

time_grid <- seq(0, T, delta)[-1]
final_payoff <- max(tail(Price_seq,1) - K, 0)

##  full
mean_var_full <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa =  1.83131, sigma = 0.00555013, theta = -0.000803381, time_grid = time_grid)
mean_var_full$phi[,1][1] <- 0 #Unstable 

##  limit
mean_var_limit <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa = 0.9950807, sigma = 0.01094479 , theta = -0.0005876739, time_grid = time_grid)
mean_var_limit$phi[,1][1] <- 0 #Unstable 

#Call prices
call_prices <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_full$H[,1], fill = "Mean-Var"), color = "Blue") +
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_limit$H[,1], fill = "Mean-Varlimit"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(Price_seq)-1), xmax = 1:(length(Price_seq)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("Mean-Var limit", "Mean-Var", "S(t) - K"),
                    breaks = c("Mean-Varlimit", "Mean-Var", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))


#Minute
hedge_1min <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_full$phi[,1], mean_var_full$H[1,1])

#10 Minute
Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

hedge_10min <- mean_var_full$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min, mean_var_full$H[1,1])
hedge_10min <- apply(hedge_10min$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour
Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

hedge_hour <- mean_var_full$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour,  mean_var_full$H[1,1])
hedge_hour <- apply(hedge_hour$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min[1:6647], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour[1:6647], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min,1) - final_payoff)
abs(tail(hedge_hour,1) - final_payoff)


#Limit

#Minute
hedge_1min_limit <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_limit$phi[,1], mean_var_limit$H[1,1])

#10 Minute

hedge_10min_limit <- mean_var_limit$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min_limit <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min_limit, mean_var_limit$H[1,1])
hedge_10min_limit <- apply(hedge_10min_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour

hedge_hour_limit <- mean_var_limit$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour_limit <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour_limit,  mean_var_limit$H[1,1])
hedge_hour_limit <- apply(hedge_hour_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot_limit <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min_limit[1:6647], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min_limit$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour_limit[1:6647], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min_limit$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min_limit,1) - final_payoff)
abs(tail(hedge_hour_limit,1) - final_payoff)

ggsave(filename = "mean_var_hedge_plot_2008_pre.png", plot = mean_var_hedge_plot, dpi = 200,
       height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_hedge_plot_2008_pre_limit.png", plot = mean_var_hedge_plot_limit, dpi = 200,
       height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_call_2008_pre.png", plot = call_prices, dpi = 200,
       height = 2.3, width = 6, units = "in")


# 2008 after crash  --------------------------------------------------------------
rm(list = ls())
source("Code_Library/functions.R")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

Price_seq <- subset(SP500_2008_cleaned_n_synced, as.Date(Date, tz = "UTC") >= as.Date("2008-12-01", tz = "UTC") &
                      as.Date(Date, tz = "UTC") < as.Date("2008-12-20", tz = "UTC"))

Datetime_seq <- Price_seq$Date
Price_seq <- Price_seq$Price
delta <- 1/60
expir_T <-  length(Price_seq) * delta
T <-  length(Price_seq) * delta
K <- 89.9
S_0 <- 89.9

time_grid <- seq(0, T, delta)[-1]
final_payoff <- max(tail(Price_seq,1) - K, 0)

##  full
mean_var_full <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa = 2.922784, sigma =  0.00858809, theta =   -0.00115692, time_grid = time_grid)
mean_var_full$phi[,1][1] <- 0 #Unstable 

##  limit
mean_var_limit <- variance_optimal_hedge(
  C_0 = NA, S = Price_seq, expir_T = expir_T, K = K,
  delta = delta, kappa = 0.1988205, sigma =  0.01628666, theta =  -0.001662, time_grid = time_grid)
mean_var_limit$phi[,1][1] <- 0 #Unstable 

#Call prices


call_prices <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_full$H[,1], fill = "Mean-Var"), color = "Blue") +
  geom_line(aes(x = 0:(length(Price_seq)-1), y = mean_var_limit$H[,1], fill = "Mean-Varlimit"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(Price_seq)-1), xmax = 1:(length(Price_seq)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("Mean-Var limit", "Mean-Var", "S(t) - K"),
                    breaks = c("Mean-Varlimit", "Mean-Var", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

#Minute
hedge_1min <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_full$phi[,1], mean_var_full$H[1,1])

#10 Minute
Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

hedge_10min <- mean_var_full$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min, mean_var_full$H[1,1])
hedge_10min <- apply(hedge_10min$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour
Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

hedge_hour <- mean_var_full$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour,  mean_var_full$H[1,1])
hedge_hour <- apply(hedge_hour$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min[1:7429], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour[1:7429], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min,1) - final_payoff)
abs(tail(hedge_hour,1) - final_payoff)


#Limit

#Minute
hedge_1min_limit <- delta_hedge(Price_seq = Price_seq, delta_seq = mean_var_limit$phi[,1], mean_var_limit$H[1,1])

#10 Minute

hedge_10min_limit <- mean_var_limit$phi[,1][seq(1,length(Price_seq),10)]
hedge_10min_limit <- delta_hedge(Price_seq = Price_10min, delta_seq = hedge_10min_limit, mean_var_limit$H[1,1])
hedge_10min_limit <- apply(hedge_10min_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 10)}) %>% c()

#Hour

hedge_hour_limit <- mean_var_limit$phi[,1][seq(1,length(Price_seq),60)]
hedge_hour_limit <- delta_hedge(Price_seq = Price_hour, delta_seq = hedge_hour_limit,  mean_var_limit$H[1,1])
hedge_hour_limit <- apply(hedge_hour_limit$portfolio_val %>% data.frame(), MARGIN = c(1,2), function(input){rep(input, 60)}) %>% c()


mean_var_hedge_plot_limit <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_10min_limit[1:7429], color = "10Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_1min_limit$portfolio_val, color = "1Min")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = hedge_hour_limit[1:7429], color = "1Hour")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = final_payoff, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("10Min", "1Min", "1Hour", expression(phi(S(T), K))),
                     breaks = c("10Min", "1Min", "1Hour", "S(T) - K"),
                     values = c("Blue", "Red", "Darkgreen", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))

abs(hedge_1min_limit$portfolio_val[length(Price_seq)-1] - final_payoff)
abs(tail(hedge_10min_limit,1) - final_payoff)
abs(tail(hedge_hour_limit,1) - final_payoff)

ggsave(filename = "mean_var_hedge_plot_2008_post.png", plot = mean_var_hedge_plot, dpi = 200,
       height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_hedge_plot_2008_post_limit.png", plot = mean_var_hedge_plot_limit, dpi = 200,
       height = 2.3, width = 6, units = "in")

ggsave(filename = "mean_var_call_2008_post.png", plot = call_prices, dpi = 200,
       height = 2.3, width = 6, units = "in")