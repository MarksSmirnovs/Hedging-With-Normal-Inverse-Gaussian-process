# Sources -----------------------------------------------------------------
source("Code_Library/functions.R")
load("Code_Library/SP500_2003_cleaned_n_sync.Rdata")

# Parameters --------------------------------------------------------------
Price_seq <- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-12-01") &
                                                      as.Date(Date) < as.Date("2003-12-20")) %>% 
  .$Price

Datetime_seq <- SP500_2003_cleaned_n_synced %>% subset(as.Date(Date) >= as.Date("2003-12-01") &
                                                         as.Date(Date) < as.Date("2003-12-20")) %>% 
  .$Date

delta <- 1/60
T <-  length(Price_seq) * delta
h <- -7.749488
K <- 106.51
S_0 <- 106.51

time_grid <- seq(0, T, delta)[-1]


# Call option price with NIG + comparison   -----------------------------------------------------------------------
# whole_M <- c()
# M <- 100
# pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0) 
# for(i in 1:M){ #30 min
# call_prices_NIG <- lapply(X = 1000,
#                           FUN = function(input){
#                             option_pricing_path(
#                               sigma = 0.004017535, theta = 0.000117011,
#                               kappa = 0.1079041, T = T, delta = delta, h = h,
#                               K = K, M = input, r = 0,
#                               Sub_NIG_sim = Sub_NIG_sim, IG = IG, NIG_MGF = NIG_MGF,
#                               Euro_Call_payoff = Euro_Call_payoff, S_0 = Price_seq,
#                               to_mean = FALSE,
#                               first_price = TRUE)
#                           })
# call_prices_NIG <- call_prices_NIG %>% unlist()
# whole_M <- c(whole_M, call_prices_NIG )
# pbapply::setTimerProgressBar(pb,i)
# }
# call_prices_NIG <- whole_M
# save(call_prices_NIG, file = "Code_Library/Delta_hedging/Call_price_NIG_100000.Rdata", compress = "xz")
load("Code_Library/Delta_hedging/Call_price_NIG_100000.Rdata")

call_prices_NIG <-  cumsum(call_prices_NIG)/seq_along(call_prices_NIG)

Call_price_NIG_M_1 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG), y = call_prices_NIG)) + 
  ylab("Call Price") + xlab("M") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG)),
                     labels = c(1, "100000"))

Call_price_NIG_M_2 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[1000:100000]), y = call_prices_NIG[1000:100000])) + 
  ylab("Call Price") + xlab("M") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[1000:100000])),
                     labels = c(1000, "100000"))

Call_price_NIG_M_3 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[10000:100000]), y = call_prices_NIG[10000:100000])) + 
  ylab("Call Price") + xlab("M") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[10000:100000])),
                     labels = c(10000, "100000"))


Call_price_NIG_M_4 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[50000:100000]), y = call_prices_NIG[50000:100000])) + 
  ylab("Call Price") + xlab("M") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[50000:100000])),
                     labels = c(50000, "100000"))

Call_price_NIG_M <- grid.arrange(Call_price_NIG_M_1, Call_price_NIG_M_2, 
                                 Call_price_NIG_M_3, Call_price_NIG_M_4)
ggsave("Call_price_NIG_M.png", Call_price_NIG_M, device = "png", dpi = 320)