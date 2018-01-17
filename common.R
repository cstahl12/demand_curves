
growVector <- function(x, g, n){
  v <- x[length(x)]
  vect <- c(x, v*(1+g))
  
  if(length(vect) < n){
    vect <- growVector(vect, g, n)
  }
  return(vect)
}

createSCurve <- function(year, peak, T1, V1, T2, V2){
  alpha <- (log(1/V1-1)-log(1/V2-1))/(T2-T1)
  t0 <- (log(1/V1-1)/alpha) + T1
  scurve <- peak / (1+exp(-alpha*(year-t0)))
  return(scurve)
}