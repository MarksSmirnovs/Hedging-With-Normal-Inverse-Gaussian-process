rm(list = ls())

library(sde)


n <- 100
sig <- 1
r <- 0
M <- 1000
K <- 100
Ti <- 1

set.seed(1)
price <- GBM(x = 100,T= Ti, r = r, sigma = sig, N = n-1)
plot(price)

time_grid <- seq(0,1, length.out = n+1)


option_price <- rep(0,n)
X_T <- numeric()
for (i in 1:n) {
  t <- time_grid[i]
  x <- price[i]
  for (m in 1:M) {
    X <- sde.sim(t0=t, X0=x, T=Ti, N=n, model = 'BS', theta = c(r, sig))
    X_T[m] <- max(X[n+1]-K,0)
  }
  F_func <- exp(-r*(Ti-i))* mean(X_T)
  option_price[i] <- F_func
}

plot(option_price, type ='l')

plot(diff(price))


length(price)
length(option_price)

delta_ratio <- (option_price[-1] - option_price[-n])/(price[-1] - price[-n])
diff(delta_ratio)*price[-1]

rebalance <- diff(delta_ratio)


portefolio <- rebalance*price[-1:-2]
portefolio <- c(delta_ratio[1]*price[2], portefolio)

option_value <- ifelse(price[-1] - K < 0,0,price[-1] - K)

sum(portefolio - option_value)

price - K

sum(portefolio-option_value)
