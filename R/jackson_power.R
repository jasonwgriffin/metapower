jackson_power <- function(k, effect_size, variance, i2, c_alpha){

  ## Compute Non-centrality parameter
  ncp <- effect_size/(sqrt(variance/k))

  ## Fixed power: Formula from Hedges Piggott
  fixed_power <- (1-pnorm(c_alpha - ncp)) + pnorm(-1*c_alpha - ncp)

  ## Random Power: Formula from Jackson & Turner (2017)

  if(is.null(i2)){
    random_power <- NA # if the user does not enter an i2 value, then mark as NA
  } else {

    t <- c_alpha # copy critical value for integral function 'func'

    ## Compute power for a range of standard heterogeneity estimates for i2 (i.e., small = 25%, moderate = 50%, large = 75%)
    random_power <- 1 - (CDF(c_alpha, k, ncp, i2) - CDF(-c_alpha, k, ncp, i2))
    random_power_0 <- 1 - (CDF(c_alpha, k, ncp, 0) - CDF(-c_alpha, k, ncp, 0))
    random_power_25 <- 1 - (CDF(c_alpha, k, ncp, .25) - CDF(-c_alpha, k, ncp, .25))
    random_power_50 <- 1 - (CDF(c_alpha, k, ncp, .50) - CDF(-c_alpha, k, ncp, .50))
    random_power_75 <- 1 - (CDF(c_alpha, k, ncp, .75) - CDF(-c_alpha, k, ncp, .75))
    random_power_100 <- 1 - (CDF(c_alpha, k, ncp, 1) - CDF(-c_alpha, k, ncp, 1))
  }
  return(list(fixed_power = fixed_power,
              random_power = random_power,
              random_power_0 = random_power_0,
              random_power_25 = random_power_25,
              random_power_50 = random_power_50,
              random_power_75 = random_power_75,
              random_power_100 = random_power_100))
}
