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

# growth assumption beyond forecast
market <- growVector(market_begin, 0.07, length(market_begin) + 9)

n <- 1000000
t <- 7
p <- runif(n, 0.001, 0.5)
t1 <- runif(n, 2018, 2020)
v1 <- runif(n, 0.05, 0.2)
t2 <- runif(n, 2021, 2025)
v2 <- runif(n, 0.8, 0.9)

demand <- market[t] * createSCurve(years[t], p, t1, v1, t2, v2)

supply <- c(0,0,0,0,1000,1125,1250,0,0,1500,0,0,0,0,0)
hodl <- runif(n, 0.05, 0.15)
velocity <- runif(n, 4, 10)
disc <- runif(n, 0.16, 0.5)

capacity <- supply[t] * (1-hodl) * velocity
fv <- demand / capacity
prices <- fv / (1+disc)^(t-1)

ggplot() + geom_histogram(aes(prices), bins=1000, alpha=0.75) + labs(x="Price", y="Frequency")
