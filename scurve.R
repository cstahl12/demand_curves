createSCurve <- function(year, peak, T1, V1, T2, V2){
  #T1 <- 2021
  #V1 <- 0.1
  #T2 <- 2025
  #V2 <- 0.8
  #peak <- .03
  #year <- c(2016:2050)
  
  alpha <- (log(1/V1-1)-log(1/V2-1))/(T2-T1)
  t0 <- (log(1/V1-1)/alpha) + T1
  scurve <- peak / (1+exp(-alpha*(year-t0)))
  return(scurve)
}
