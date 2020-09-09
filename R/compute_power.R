compute_power <- function(k, effect_size, variance, c_alpha, test_type){

  ## Compute Non-centrality parameter
  ncp <- effect_size/(sqrt(variance/k))
  t <- c_alpha # copy critical value for integral function 'func'
  ## Fixed power: Formula from Hedges Piggot

  if(test_type =="two-tailed"){
    fixed_power <- (1-pnorm(c_alpha - ncp)) + pnorm(-1*c_alpha - ncp)
    random_power_0 <- 1 - (CDF(c_alpha, k, ncp, 0) - CDF(-c_alpha, k, ncp, 0))
    random_power_25 <- 1 - (CDF(c_alpha, k, ncp, .25) - CDF(-c_alpha, k, ncp, .25))
    random_power_50 <- 1 - (CDF(c_alpha, k, ncp, .50) - CDF(-c_alpha, k, ncp, .50))
    random_power_75 <- 1 - (CDF(c_alpha, k, ncp, .75) - CDF(-c_alpha, k, ncp, .75))
  } else {
    fixed_power <- 1 - pnorm(c_alpha - ncp)
    random_power_0 <- 1 - (CDF(c_alpha, k, ncp, 0))
    random_power_25 <- 1 - (CDF(c_alpha, k, ncp, .25))
    random_power_50 <- 1 - (CDF(c_alpha, k, ncp, .50))
    random_power_75 <- 1 - (CDF(c_alpha, k, ncp, .75))

  }


  ## Random Power: Formula from Jackson & Turner (2017



  ## Compute power for a range of standard heterogeneity estimates for i2 (i.e., small = 25%, moderate = 50%, large = 75%


  return(list(fixed_power = fixed_power,
              random_power_0 = random_power_0,
              random_power_25 = random_power_25,
              random_power_50 = random_power_50,
              random_power_75 = random_power_75))
}
