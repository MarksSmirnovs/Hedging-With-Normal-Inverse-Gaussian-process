rm(list = ls())

source("Code_Library/functions.R")

lambda <- 1
tMax <- 10

## find the number 'n' of exponential r.vs required by imposing that
## Pr{N(t) <= n} <= 1 - eps for a small 'eps'
n <- qpois(1 - 1e-8, lambda = lambda * tMax)

## simulate exponential interarrivals the
X <- rexp(n = n, rate = lambda)
S <- c(0, cumsum(X))
plot(x = S, y = 0:n, type = "s", xlim = c(0, tMax)) 

plot(x = S - (0:n)*lambda, type = 'l')

x2 <- c(0,cumsum(rnorm(n)))

gg_pp <- data.frame(x = S, x2 = x2, y = 0:n)

gg_point <- data.frame(x= S, y = seq.int(0,32, by = 1), x2 = c(S[-1], S[n]), y2 = x2)

pois_pross <- ggplot(data = gg_pp, aes(x = x, y = y)) +
  geom_step()+
  xlim(0,tMax)+
  ylim(0,15)+
  xlab('t')+
  ylab('N(t)')+
  geom_point(data = gg_point, aes(x = x, y = y))+
  geom_point(data = gg_point, aes(x = x2, y = y), shape = 1)


 # ggsave(filename = "lambda_1.png", plot = pois_pross, dpi = 200,
 #       height = 2.3, width = 6, units = "in")
 
compound_poisson <- ggplot(data = gg_pp, aes(x =S, y = x2)) +
  geom_step()+
  ylab('X(t)')+
  xlab('t')+
  xlim(0,tMax)+
  geom_point(data = gg_point, aes(x = x, y = y2))+
  geom_point(data = gg_point, aes(x = x2, y = y2), shape = 1)




plot(stepfun(S[-1], x2))

plot(S, x2, type = 'b')
