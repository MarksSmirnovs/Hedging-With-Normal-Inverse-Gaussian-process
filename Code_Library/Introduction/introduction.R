rm(list = ls())

source("Code_Library/functions.R")

load("Code_Library/SP500_2003.Rdata")

Time = 100
sp500_2003_split <- sp500_2003 %>% filter(date < "2003-01-07")


t <- 0:nrow(sp500_2003_split)
sigma <- 0.0005#sd(sp500_2003_split$price)
b  <- 0#log(sp500_2003_split$price[nrow(sp500_2003_split)] / sp500_2003_split$price[1]) / Time
set.seed(12)
N <- rnorm(length(t), mean = 0, sd = 1)

# delta_X <- NULL
# for(i in 2:length(t)){
#   delta_X[i-1] <- sigma * N[i] * sqrt(t[i] - t[i-1]) + b * (t[i] - t[i-1])
# }
# 
# X <- cumsum(delta_X)
# plot(X)

S_0 <- sp500_2003$price[1]

X <- NULL
for(i in 2:length(t)){

  X[i-1] <- exp(sigma * N[i] * sqrt(t[i] - t[i-1]) + b * (t[i] - t[i-1]))
  
  }

X <- cumprod(X)
X <- S_0 * X


gbm_week <- ggplot(mapping = aes(x = 1:length(X), y = X)) + 
  geom_point(size = 0.85) + ylab("Geometric Brownian Motion") + xlab(NULL) + theme(axis.ticks.x=element_blank(), 
                                                          axis.text.x=element_blank())


sp500_week <- ggplot(mapping = aes(x = 1:length(sp500_2003_split$price), y = sp500_2003_split$price)) + 
  geom_point(size = 0.85) + ylim(88, 94) + ylab("SP500 Price") + xlab(NULL) + theme(axis.ticks.x=element_blank(), 
                                                                         axis.text.x=element_blank())
# ggsave(filename = "intro_gbm_week.png", plot = gbm_week, dpi = 200,
#        height = 2.3, width = 6, units = "in")

# ggsave(filename = "intro_sp500_week.png", plot = sp500_week, dpi = 200,
#        height = 2.3, width = 6, units = "in")

gridExtra::grid.arrange(gbm_week, sp500_week, ncol = 2)

intra_day_index <- which(sp500_2003$date >= "2003-01-02 10:50:00" & sp500_2003$date < "2003-01-02 11:00:00" )

gbm_10min <- ggplot(mapping = aes(x = 1:length(X[intra_day_index]), y = X[intra_day_index])) + 
  geom_point(size = 0.85) + ylab("Geometric Brownian Motion") + xlab(NULL) + theme(axis.ticks.x=element_blank(), 
                                                                    axis.text.x=element_blank())


sp500_10min <- ggplot(mapping = aes(x = 1:length(sp500_2003_split$price[intra_day_index]), y = sp500_2003_split$price[intra_day_index])) + 
  geom_point(size = 0.85)+ ylab("SP500 Price") + xlab(NULL) + theme(axis.ticks.x=element_blank(), 
                                                                         axis.text.x=element_blank())

# ggsave(filename = "intro_sp500_10min.png", plot = sp500_10min, dpi = 200,
#        height = 2.3, width = 6, units = "in")
