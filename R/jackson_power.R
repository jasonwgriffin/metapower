jackson_power <- function(k, effect_size, variance, i2){

  if(is.null(i2)){
    random_power <- NA
  } else {

    Z <- qnorm(0.975) ## alpha value
    t <- Z
    ncp <- effect_size*sqrt(k)/sqrt(variance)
    random_power <- 1 - (CDF(Z, k, ncp, i2) - CDF(-Z, k, ncp, i2))
    random_power_0 <- 1 - (CDF(Z, k, ncp, 0) - CDF(-Z, k, ncp, 0))
    random_power_25 <- 1 - (CDF(Z, k, ncp, .25) - CDF(-Z, k, ncp, .25))
    random_power_50 <- 1 - (CDF(Z, k, ncp, .50) - CDF(-Z, k, ncp, .50))
    random_power_75 <- 1 - (CDF(Z, k, ncp, .75) - CDF(-Z, k, ncp, .75))
    random_power_100 <- 1 - (CDF(Z, k, ncp, 1) - CDF(-Z, k, ncp, 1))
  }
  return(list(random_power = random_power,
              random_power_0 = random_power_0,
              random_power_25 = random_power_25,
              random_power_50 = random_power_50,
              random_power_75 = random_power_75,
              random_power_100 = random_power_100))
}
