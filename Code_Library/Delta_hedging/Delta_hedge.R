# Delta hedge 2003 --------------------------------------------------------

# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")

# Parameters --------------------------------------------------------------

Price_seq <- subset(SP500_2003_cleaned_n_synced, as.Date(Date, tz = "UTC") >= as.Date("2003-12-01", tz = "UTC") &
                      as.Date(Date, tz = "UTC") < as.Date("2003-12-20", tz = "UTC"))



Datetime_seq <- Price_seq$Date
Price_seq <- Price_seq$Price

delta <- 1/60
T <-  length(Price_seq) * delta
h <- -7.749488
K <- 106.51
S_0 <- 106.51

time_grid <- seq(0, T, delta)[-1]

# NIG call prices ---------------------------------------------------------
# 
# NIG_Call_Prices <- 0
# M <- 100
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #1 hour 7 min
# NIG_Call_Prices_single <- option_pricing_path(
#   sigma = 0.004017535, theta = 0.000117011,
#   kappa = 0.1079041, T = T, delta = delta, h = h,
#   K = K, M = 1000, r = 0,
#   Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#   Euro_Call_payoff = Euro_Call_payoff, S_0 = Price_seq, to_mean = FALSE)
# 
# NIG_Call_Prices_single <- apply(NIG_Call_Prices_single, MARGIN = 1, FUN = function(input){
#   sum(input)/(M*1000)
# })
# 
# NIG_Call_Prices <- NIG_Call_Prices + NIG_Call_Prices_single
# 
# pbapply::setTimerProgressBar(pb,i)
# }
# save(NIG_Call_Prices, file = "NIG_Call_Prices_2003.Rdata")

load("Code_Library/Delta_hedging/NIG_Call_Prices_2003.Rdata")
# Black Scholes Call prices -----------------------------------------------


BS_sigma <- 0.00462 


BS_Call_Prices <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_Call(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


# Comparing ---------------------------------------------------------------

NIG_BS_Call_prices_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = BS_Call_Prices, fill = "BS Price"), color = "Blue") + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = NIG_Call_Prices, fill = "NIG Price"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(BS_Call_Prices)-1), xmax = 1:(length(BS_Call_Prices)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("BS Price", "NIG Price", "S(t) - K"),
                    breaks = c("BS Price", "NIG Price", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(BS_Call_Prices)/2), length(BS_Call_Prices)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]))

# HR_Png(name = "NIG_BS_Call_prices_minute", figure = NIG_BS_Call_prices_minute)

# ggsave(filename = "NIG_BS_Call_prices_minute.png", plot = NIG_BS_Call_prices_minute, dpi = 200,
#        height = 2.3, width = 6, units = "in")

# Delta Hedging ------------------------------------------------------------

#Calculate the deltas
ext_price_seq <- c(subset(SP500_2003_cleaned_n_synced, Date == as.POSIXct("2003-11-28 16:00:00", tz = "UTC"))$Price,
                   Price_seq[1:(length(Price_seq)-1)])


# Extra_NIG_Price <- NULL
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #29min
# Extra_NIG_Price_single <-   option_pricing_path(
#   sigma = 0.004017535, theta = 0.000117011,
#   kappa = 0.1079041, T = length(Price_seq+1) * delta, delta = delta, h = h,
#   K = K, M = 1000, r = 0,
#   Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#   Euro_Call_payoff = Euro_Call_payoff, S_0 = ext_price_seq,
#   first_price = TRUE) %>% unlist()
# 
# Extra_NIG_Price <- c(Extra_NIG_Price, Extra_NIG_Price_single)
# 
# pbapply::setTimerProgressBar(pb,i)
# }
# Extra_NIG_Price <- Extra_NIG_Price %>% mean()
# #1.618114
Extra_NIG_Price <- 1.618114


ext_NIG_Prices <- c(
  Extra_NIG_Price,
  NIG_Call_Prices[1:(length(Price_seq)-1)]
)

ext_BS_Prices <- c(
  BS_Call(S = ext_price_seq[1], K = K, r = 0, T = length(ext_price_seq+1) * delta, sigma = BS_sigma),
  BS_Call_Prices[1:(length(Price_seq)-1)]
)

BS_Call_delta <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_delta(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


NIG_Call_delta <- backward_finite_diff(S_1 = Price_seq, 
                                       S_0 = ext_price_seq,
                                       C_1 = NIG_Call_Prices,
                                       C_0 = ext_NIG_Prices)

# Minute hedge ------------------------------------------------------------


BS_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = BS_Call_delta, BS_Call_Prices[1])
NIG_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = NIG_Call_delta, NIG_Call_Prices[1])

delta_comparison_BS_NIG_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = BS_delta_hedge$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = NIG_delta_hedge$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = 2.49, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", expression(phi(S(T), K))),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]))


# HR_Png(name = "delta_comparison_BS_NIG_minute", figure = delta_comparison_BS_NIG_minute)
# ggsave(filename = "delta_comparison_BS_NIG_minute.png", plot = delta_comparison_BS_NIG_minute, dpi = 200,
 #       height = 2.3, width = 6, units = "in")
Datetime_seq[length(Price_seq)-1]
abs(BS_delta_hedge$portfolio_val[length(Price_seq)-1] - 2.49)
abs(NIG_delta_hedge$portfolio_val[length(Price_seq)-1] - 2.49)



# 10 Minute hedge ---------------------------------------------------------

Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

NIG_delta_10min <- NIG_Call_delta[seq(1,length(Price_seq),10)]
BS_delta_10min <- BS_Call_delta[seq(1,length(Price_seq),10)]


BS_delta_hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = BS_delta_10min, BS_Call_Prices[1])
NIG_delta_hedge_10min <-delta_hedge(Price_seq = Price_10min, delta_seq = NIG_delta_10min, NIG_Call_Prices[1])

delta_comparison_BS_NIG_10min <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = BS_delta_hedge_10min$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = NIG_delta_hedge_10min$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_10min)-1), y = 2.49, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", expression(phi(S(T), K))),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_10min)/2), length(Price_10min)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]))


# HR_Png(name = "delta_comparison_BS_NIG_10min", figure = delta_comparison_BS_NIG_10min)
 #ggsave(filename = "delta_comparison_BS_NIG_10min.png", plot = delta_comparison_BS_NIG_10min, dpi = 200,
  #      height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),10) %>% tail(1)]
abs(tail(BS_delta_hedge_10min$portfolio_val,1) - 2.49)
abs(tail(NIG_delta_hedge_10min$portfolio_val,1) - 2.49)



# Hourly hedge ------------------------------------------------------------

Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

NIG_delta_hour <- NIG_Call_delta[seq(1,length(Price_seq),60)]
BS_delta_hour <- BS_Call_delta[seq(1,length(Price_seq),60)]


BS_delta_hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = BS_delta_hour, BS_Call_Prices[1])
NIG_delta_hedge_hour <-delta_hedge(Price_seq = Price_hour, delta_seq = NIG_delta_hour, NIG_Call_Prices[1])


delta_comparison_BS_NIG_hour <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = BS_delta_hedge_hour$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = NIG_delta_hedge_hour$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_hour)-1), y = 2.49, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", expression(phi(S(T), K))),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime")    + 
  scale_x_continuous(breaks = c(0, round(length(Price_hour)/2), length(Price_hour)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]))


# HR_Png(name = "delta_comparison_BS_NIG_hour", figure = delta_comparison_BS_NIG_hour)
 #ggsave(filename = "delta_comparison_BS_NIG_hour.png", plot = delta_comparison_BS_NIG_hour, dpi = 200,
  #      height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),60) %>% tail(1)]
abs(tail(BS_delta_hedge_hour$portfolio_val,1) - 2.49)
abs(tail(NIG_delta_hedge_hour$portfolio_val,1) - 2.49)


# Delta hedge 2008 during crash -------------------------------------------

rm(list = ls())
# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

# Parameters --------------------------------------------------------------

Price_seq <- subset(SP500_2008_cleaned_n_synced, as.Date(Date, tz = "UTC") >= as.Date("2008-10-01", tz = "UTC") &
                      as.Date(Date, tz = "UTC") < as.Date("2008-10-18", tz = "UTC"))



Datetime_seq <- Price_seq$Date
Price_seq <- Price_seq$Price

delta <- 1/60
T <-  length(Price_seq) * delta
h <- 25.58039

#SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) < "2008-10-01") %>% tail()
K <- 115.94
S_0 <- 115.94

time_grid <- seq(0, T, delta)[-1]

# NIG call prices ---------------------------------------------------------
# 
# NIG_Call_Prices <- 0
# M <- 100
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #1 hour 7 min
# NIG_Call_Prices_single <- option_pricing_path(
#   sigma = 0.005550139, theta = -0.0008033815,
#   kappa = 1.831318, T = T, delta = delta, h = h,
#   K = K, M = 1000, r = 0,
#   Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#   Euro_Call_payoff = Euro_Call_payoff, S_0 = Price_seq, to_mean = FALSE)
# 
# NIG_Call_Prices_single <- apply(NIG_Call_Prices_single, MARGIN = 1, FUN = function(input){
#   sum(input)/(M*1000)
# })
# 
# NIG_Call_Prices <- NIG_Call_Prices + NIG_Call_Prices_single
# 
# pbapply::setTimerProgressBar(pb,i)
# }
# save(NIG_Call_Prices, file = "NIG_Call_Prices_2008_option1.Rdata")

load("Code_Library/Delta_hedging/NIG_Call_Prices_2008_option1.Rdata")
# Black Scholes Call prices -----------------------------------------------

BS_sigma <- 0.00478 



BS_Call_Prices <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_Call(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


# Comparing ---------------------------------------------------------------

NIG_BS_Call_prices_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = BS_Call_Prices, fill = "BS Price"), color = "Blue") + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = NIG_Call_Prices, fill = "NIG Price"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(BS_Call_Prices)-1), xmax = 1:(length(BS_Call_Prices)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("BS Price", "NIG Price", "S(t) - K"),
                    breaks = c("BS Price", "NIG Price", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(BS_Call_Prices)/2), length(BS_Call_Prices)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));NIG_BS_Call_prices_minute

# HR_Png(name = "NIG_BS_Call_prices_minute", figure = NIG_BS_Call_prices_minute)

 # ggsave(filename = "crash_NIG_BS_Call_prices_minute.png", plot = NIG_BS_Call_prices_minute, dpi = 200,
 #        height = 2.3, width = 6, units = "in")

# Delta Hedging ------------------------------------------------------------

#Calculate the deltas
ext_price_seq <- c(subset(SP500_2008_cleaned_n_synced, Date == as.POSIXct("2008-09-30 16:00:00", tz = "UTC"))$Price,
                   Price_seq[1:(length(Price_seq)-1)])


# Extra_NIG_Price <- NULL
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #29min
# Extra_NIG_Price_single <-   option_pricing_path(
#   sigma = 0.005550139 , theta = -0.0008033815,
#   kappa = 1.831318, T = length(Price_seq+1) * delta, delta = delta, h = h,
#   K = K, M = 1000, r = 0,
#   Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#   Euro_Call_payoff = Euro_Call_payoff, S_0 = ext_price_seq,
#   first_price = TRUE) %>% unlist()
# 
# Extra_NIG_Price <- c(Extra_NIG_Price, Extra_NIG_Price_single)
# 
# pbapply::setTimerProgressBar(pb,i)
# }
# Extra_NIG_Price <- Extra_NIG_Price %>% mean()
# # #0.8576974
Extra_NIG_Price <- 0.8576974


ext_NIG_Prices <- c(
  Extra_NIG_Price,
  NIG_Call_Prices[1:(length(Price_seq)-1)]
)

ext_BS_Prices <- c(
  BS_Call(S = ext_price_seq[1], K = K, r = 0, T = length(ext_price_seq+1) * delta, sigma = BS_sigma),
  BS_Call_Prices[1:(length(Price_seq)-1)]
)

BS_Call_delta <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_delta(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


NIG_Call_delta <- backward_finite_diff(S_1 = Price_seq, 
                                       S_0 = ext_price_seq,
                                       C_1 = NIG_Call_Prices,
                                       C_0 = ext_NIG_Prices)

# Minute hedge ------------------------------------------------------------


BS_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = BS_Call_delta, BS_Call_Prices[1])
NIG_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = NIG_Call_delta, NIG_Call_Prices[1])

pay.off <- max(tail(ext_price_seq,1) - K,0)

delta_comparison_BS_NIG_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = BS_delta_hedge$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = NIG_delta_hedge$portfolio_val, color = "NIG Value")) +
  geom_line(aes(x =  0:(length(Price_seq)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", expression(phi(S(T), K))),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]));delta_comparison_BS_NIG_minute


# HR_Png(name = "delta_comparison_BS_NIG_minute", figure = delta_comparison_BS_NIG_minute)
   #ggsave(filename = "crash_delta_comparison_BS_NIG_minute.png", plot = delta_comparison_BS_NIG_minute, dpi = 200,
    #       height = 2.3, width = 6, units = "in")
Datetime_seq[length(Price_seq)-1]
abs(BS_delta_hedge$portfolio_val[length(Price_seq)-1] - pay.off)
abs(NIG_delta_hedge$portfolio_val[length(Price_seq)-1] - pay.off)



# 10 Minute hedge ---------------------------------------------------------

Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

NIG_delta_10min <- NIG_Call_delta[seq(1,length(Price_seq),10)]
BS_delta_10min <- BS_Call_delta[seq(1,length(Price_seq),10)]


BS_delta_hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = BS_delta_10min, BS_Call_Prices[1])
NIG_delta_hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = NIG_delta_10min, NIG_Call_Prices[1])

delta_comparison_BS_NIG_10min <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = BS_delta_hedge_10min$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = NIG_delta_hedge_10min$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_10min)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", "S(T) - K"),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_10min)/2), length(Price_10min)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));delta_comparison_BS_NIG_10min


# HR_Png(name = "delta_comparison_BS_NIG_10min", figure = delta_comparison_BS_NIG_10min)
 # ggsave(filename = "crash_delta_comparison_BS_NIG_10min.png", plot = delta_comparison_BS_NIG_10min, dpi = 200,
 #         height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),10) %>% tail(1)]
abs(tail(BS_delta_hedge_10min$portfolio_val,1) - pay.off)
abs(tail(NIG_delta_hedge_10min$portfolio_val,1) - pay.off)



# Hourly hedge ------------------------------------------------------------

Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

NIG_delta_hour <- NIG_Call_delta[seq(1,length(Price_seq),60)]
BS_delta_hour <- BS_Call_delta[seq(1,length(Price_seq),60)]


BS_delta_hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = BS_delta_hour, BS_Call_Prices[1])
NIG_delta_hedge_hour <-delta_hedge(Price_seq = Price_hour, delta_seq = NIG_delta_hour, NIG_Call_Prices[1])


delta_comparison_BS_NIG_hour <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = BS_delta_hedge_hour$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = NIG_delta_hedge_hour$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_hour)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", "S(T) - K"),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime")    + 
  scale_x_continuous(breaks = c(0, round(length(Price_hour)/2), length(Price_hour)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));delta_comparison_BS_NIG_hour


# HR_Png(name = "delta_comparison_BS_NIG_hour", figure = delta_comparison_BS_NIG_hour)
  # ggsave(filename = "crash_delta_comparison_BS_NIG_hour.png", plot = delta_comparison_BS_NIG_hour, dpi = 200,
  #        height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),60) %>% tail(1)]
abs(tail(BS_delta_hedge_hour$portfolio_val,1) - pay.off)
abs(tail(NIG_delta_hedge_hour$portfolio_val,1) - pay.off)

# Delta hedge 2008 after crash -------------------------------------------

rm(list = ls())
# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2008_cleaned_n_sync.Rdata")

# Parameters --------------------------------------------------------------

Price_seq <- subset(SP500_2008_cleaned_n_synced, as.Date(Date, tz = "UTC") >= as.Date("2008-12-01", tz = "UTC") &
                      as.Date(Date, tz = "UTC") < as.Date("2008-12-20", tz = "UTC"))



Datetime_seq <- Price_seq$Date
Price_seq <- Price_seq$Price

delta <- 1/60
T <-  length(Price_seq) * delta
h <- 15.18597

#SP500_2008_cleaned_n_synced %>% subset(as.Date(Date) < "2008-12-01") %>% tail()
K <- 89.9
S_0 <- 89.9

pay.off <-max(c(tail(Price_seq,1) - K), 0)
time_grid <- seq(0, T, delta)[-1]

# NIG call prices ---------------------------------------------------------
# 
# NIG_Call_Prices <- 0
# M <- 100
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #1 hour 7 min
#   NIG_Call_Prices_single <- option_pricing_path(
#     sigma = 0.008588097, theta = -0.001156925,
#     kappa = 2.922784, T = T, delta = delta, h = h,
#     K = K, M = 1000, r = 0,
#     Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#     Euro_Call_payoff = Euro_Call_payoff, S_0 = Price_seq, to_mean = FALSE)
#   
#   NIG_Call_Prices_single <- apply(NIG_Call_Prices_single, MARGIN = 1, FUN = function(input){
#     sum(input)/(M*1000)
#   })
#   
#   NIG_Call_Prices <- NIG_Call_Prices + NIG_Call_Prices_single
#   
#   pbapply::setTimerProgressBar(pb,i)
# }
# save(NIG_Call_Prices, file = "NIG_Call_Prices_2008_option2.Rdata")

load("Code_Library/Delta_hedging/NIG_Call_Prices_2008_option2.Rdata")
# Black Scholes Call prices -----------------------------------------------

BS_sigma <- 0.00725 

BS_Call_Prices <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_Call(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


# Comparing ---------------------------------------------------------------

NIG_BS_Call_prices_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = BS_Call_Prices, fill = "BS Price"), color = "Blue") + 
  geom_line(aes(x = 0:(length(BS_Call_Prices)-1), y = NIG_Call_Prices, fill = "NIG Price"), color = "Red") + 
  geom_rect(mapping= aes(xmin =  0:(length(BS_Call_Prices)-1), xmax = 1:(length(BS_Call_Prices)),
                         ymin = pmin(Price_seq-K, 0),
                         ymax = pmax(Price_seq-K, 0), fill = "S(t) - K"), position = "identity", alpha = 0.5) +
  scale_fill_manual(labels = c("BS Price", "NIG Price", "S(t) - K"),
                    breaks = c("BS Price", "NIG Price", "S(t) - K"),
                    values = c("Blue", "Red", "Black"),
                    name = "") +
  ylab("Price") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(BS_Call_Prices)/2), length(BS_Call_Prices)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));NIG_BS_Call_prices_minute

# HR_Png(name = "NIG_BS_Call_prices_minute", figure = NIG_BS_Call_prices_minute)

 # ggsave(filename = "after_NIG_BS_Call_prices_minute.png", plot = NIG_BS_Call_prices_minute, dpi = 200,
 #         height = 2.3, width = 6, units = "in")

# Delta Hedging ------------------------------------------------------------

#Calculate the deltas
ext_price_seq <- c(subset(SP500_2008_cleaned_n_synced, Date == as.POSIXct("2008-11-30 16:00:00", tz = "UTC"))$Price,
                   Price_seq[1:(length(Price_seq)-1)])

# 
# Extra_NIG_Price <- NULL
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
# for(i in 1:M){ #29min
# Extra_NIG_Price_single <-   option_pricing_path(
#   sigma =  0.008588097  , theta = -0.001156925,
#   kappa = 2.922784, T = length(Price_seq+1) * delta, delta = delta, h = h,
#   K = K, M = 1000, r = 0,
#   Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#   Euro_Call_payoff = Euro_Call_payoff, S_0 = ext_price_seq,
#   first_price = TRUE) %>% unlist()
# 
# Extra_NIG_Price <- c(Extra_NIG_Price, Extra_NIG_Price_single)
# 
# pbapply::setTimerProgressBar(pb,i)
# }
# Extra_NIG_Price <- Extra_NIG_Price %>% mean()
# # #1.197803
Extra_NIG_Price <- 1.197803


ext_NIG_Prices <- c(
  Extra_NIG_Price,
  NIG_Call_Prices[1:(length(Price_seq)-1)]
)

ext_BS_Prices <- c(
  BS_Call(S = ext_price_seq[1], K = K, r = 0, T = length(ext_price_seq+1) * delta, sigma = BS_sigma),
  BS_Call_Prices[1:(length(Price_seq)-1)]
)

BS_Call_delta <- lapply(X = 1:length(Price_seq), FUN = function(input){
  BS_delta(S = Price_seq[input], K = K, r = 0, T = rev(time_grid)[input], sigma = BS_sigma)
}) %>% do.call(what =  rbind)


NIG_Call_delta <- backward_finite_diff(S_1 = Price_seq, 
                                       S_0 = ext_price_seq,
                                       C_1 = NIG_Call_Prices,
                                       C_0 = ext_NIG_Prices)

# Minute hedge ------------------------------------------------------------


BS_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = BS_Call_delta, BS_Call_Prices[1])
NIG_delta_hedge <- delta_hedge(Price_seq = Price_seq, delta_seq = NIG_Call_delta, NIG_Call_Prices[1])

delta_comparison_BS_NIG_minute <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = BS_delta_hedge$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_seq)-1), y = NIG_delta_hedge$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_seq)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", "S(T) - K"),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_seq)/2), length(Price_seq)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(Price_seq)/2)],
                                Datetime_seq[length(Price_seq)]));delta_comparison_BS_NIG_minute

# HR_Png(name = "delta_comparison_BS_NIG_minute", figure = delta_comparison_BS_NIG_minute)
 # ggsave(filename = "after_delta_comparison_BS_NIG_minute.png", plot = delta_comparison_BS_NIG_minute, dpi = 200,
 #         height = 2.3, width = 6, units = "in")
Datetime_seq[length(Price_seq)-1]
abs(BS_delta_hedge$portfolio_val[length(Price_seq)-1] - pay.off)
abs(NIG_delta_hedge$portfolio_val[length(Price_seq)-1] - pay.off)



# 10 Minute hedge ---------------------------------------------------------

Price_10min <- Price_seq[seq(1,length(Price_seq),10)]

NIG_delta_10min <- NIG_Call_delta[seq(1,length(Price_seq),10)]
BS_delta_10min <- BS_Call_delta[seq(1,length(Price_seq),10)]


BS_delta_hedge_10min <- delta_hedge(Price_seq = Price_10min, delta_seq = BS_delta_10min, BS_Call_Prices[1])
NIG_delta_hedge_10min <-delta_hedge(Price_seq = Price_10min, delta_seq = NIG_delta_10min, NIG_Call_Prices[1])

delta_comparison_BS_NIG_10min <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = BS_delta_hedge_10min$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_10min)-1), y = NIG_delta_hedge_10min$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_10min)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", "S(T) - K"),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime") + 
  scale_x_continuous(breaks = c(0, round(length(Price_10min)/2), length(Price_10min)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));delta_comparison_BS_NIG_10min


# HR_Png(name = "delta_comparison_BS_NIG_10min", figure = delta_comparison_BS_NIG_10min)
 # ggsave(filename = "after_delta_comparison_BS_NIG_10min.png", plot = delta_comparison_BS_NIG_10min, dpi = 200,
 #        height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),10) %>% tail(1)]
abs(tail(BS_delta_hedge_10min$portfolio_val,1) - pay.off)
abs(tail(NIG_delta_hedge_10min$portfolio_val,1) - pay.off)



# Hourly hedge ------------------------------------------------------------

Price_hour <- Price_seq[seq(1,length(Price_seq),60)]

NIG_delta_hour <- NIG_Call_delta[seq(1,length(Price_seq),60)]
BS_delta_hour <- BS_Call_delta[seq(1,length(Price_seq),60)]


BS_delta_hedge_hour <- delta_hedge(Price_seq = Price_hour, delta_seq = BS_delta_hour, BS_Call_Prices[1])
NIG_delta_hedge_hour <-delta_hedge(Price_seq = Price_hour, delta_seq = NIG_delta_hour, NIG_Call_Prices[1])


delta_comparison_BS_NIG_hour <- ggplot() + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = BS_delta_hedge_hour$portfolio_val, color = "BS Value")) + 
  geom_line(aes(x = 0:(length(Price_hour)-1), y = NIG_delta_hedge_hour$portfolio_val, color = "NIG Value")) + 
  geom_line(aes(x =  0:(length(Price_hour)-1), y = pay.off, color = "S(T) - K"), linetype = "dashed") + 
  scale_color_manual(labels = c("BS Value", "NIG Value", "S(T) - K"),
                     breaks = c("BS Value", "NIG Value", "S(T) - K"),
                     values = c("Blue", "Red", "Black"),
                     name = "") +
  ylab("Replicating Portfolio Value") + xlab("Datetime")    + 
  scale_x_continuous(breaks = c(0, round(length(Price_hour)/2), length(Price_hour)),
                     labels = c(Datetime_seq[1], Datetime_seq[round(length(BS_Call_Prices)/2)],
                                Datetime_seq[length(BS_Call_Prices)]));delta_comparison_BS_NIG_hour


# HR_Png(name = "delta_comparison_BS_NIG_hour", figure = delta_comparison_BS_NIG_hour)
 # ggsave(filename = "after_delta_comparison_BS_NIG_hour.png", plot = delta_comparison_BS_NIG_hour, dpi = 200,
 #        height = 2.3, width = 6, units = "in")
Datetime_seq[seq(1,length(Price_seq),60) %>% tail(1)]
abs(tail(BS_delta_hedge_hour$portfolio_val,1) - pay.off)
abs(tail(NIG_delta_hedge_hour$portfolio_val,1) - pay.off)
