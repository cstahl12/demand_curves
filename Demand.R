require(ggplot2)

source("../SCurve/scurve.R")

growVector <- function(x, g, n){
  v <- x[length(x)]
  vect <- c(x, v*(1+g))
  
  if(length(vect) < n){
    vect <- growVector(vect, g, n)
  }
  return(vect)
}

years <- c(2017:(2017+14))

# market forecast
forecast_delta <- (92488 - 25171) / 5
market_begin <- cumsum(c(25171, forecast_delta, forecast_delta, forecast_delta, forecast_delta, forecast_delta))

# Fit (visually) total market S-curve with actual data and analyst estimates
plot_storage_market <- createSCurve(years, 150000, 2010, .01, 2025, .85)

ggplot() + geom_line(aes(x=years, y=plot_storage_market)) + 
  geom_point(aes(x=2017,y=25171)) + geom_point(aes(x=2022,y=92488))

plot_adoption <- createSCurve(years, .25, 2019, .1, 2023, .9)

ggplot() +
  geom_line(aes(x=years, y=plot_storage_market)) + 
  geom_line(aes(x=years, y=plot_adoption*plot_storage_market)) +
  geom_point(aes(x=2017,y=25171)) + geom_point(aes(x=2022,y=92488))

ggplot() +
  geom_line(aes(x=years, y=log(plot_storage_market))) + 
  geom_line(aes(x=years, y=log(plot_adoption*plot_storage_market)))

# Monte Carlo Simulation
n <- 1000000
t <- 7

# incumbent market estimate
mscurve <- createSCurve(years[t], runif(n, 125000, 175000), 2010, .01, 2025, .85)

# substitution market estimate
p <- runif(n, 0.01, 0.5)
t1 <- runif(n, 2018, 2020)
v1 <- runif(n, 0.05, 0.2)
t2 <- runif(n, 2021, 2025)
v2 <- runif(n, 0.8, 0.9)
cscurve <- createSCurve(years[t], p, t1, .1, t2, .9)

market_share <- runif(n, .4, 0.75)
demand <- mscurve * cscurve * market_share

supply <- c(0,0,0,0,1000,1125,1250,0,0,1500,0,0,0,0,0)
hodl <- runif(n, 0.05, 0.15)
velocity <- runif(n, 10, 20)
disc <- runif(n, 0.3, 0.5)

capacity <- supply[t] * (1-hodl) * velocity
fv <- demand / capacity

prices <- fv / (1+disc)^(t-1)
erp_prices <- fv / (1.15)^(t-1)
rf_prices <- fv / (1.027)^(t-1)


ggplot() +
  geom_histogram(aes(prices), bins=1000, alpha=0.75) +
  geom_vline(xintercept=median(prices)) +
  geom_vline(xintercept=mean(prices)) +
  labs(x="Price", y="Frequency") + xlim(0, 0.5)

ggplot() +
  geom_histogram(aes(erp_prices), bins=1000, alpha=0.75) +
  geom_vline(xintercept=median(erp_prices)) +
  geom_vline(xintercept=mean(erp_prices)) +
  labs(x="Price", y="Frequency") + xlim(0, 1.5)

ggplot() +
  geom_histogram(aes(rf_prices), bins=1000, alpha=0.75) +
  geom_vline(xintercept=median(rf_prices)) +
  geom_vline(xintercept=mean(rf_prices)) +
  labs(x="Price", y="Frequency")


med_price_future_utility_multiple <- 30 / median(prices)


  
