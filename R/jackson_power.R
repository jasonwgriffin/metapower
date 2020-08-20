
jackson_power <- function(k, es, sd, i2){
  Z <- qnorm(0.975) ## alpha value
  t <- Z
  ncp <- es*sqrt(k)/sd
  random_power <- 1 - (CDF(Z, k, ncp, i2) - CDF(-Z, k, ncp, i2))
  return(random_power)
}

