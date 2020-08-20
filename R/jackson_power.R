jackson_power <- function(k, effect_size, variance, i2){

  if(is.null(i2)){
    random_power <- NA
  } else {
    Z <- qnorm(0.975) ## alpha value
    t <- Z
    ncp <- effect_size*sqrt(k)/sqrt(variance)
    random_power <- 1 - (CDF(Z, k, ncp, i2) - CDF(-Z, k, ncp, i2))
  }
  return(random_power)
}
