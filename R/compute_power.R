compute_power <- function(k, effect_size, variance, i2, c_alpha, test_type){

  ncp <- effect_size/(sqrt(variance/k)) # Compute non-centrality parameter
  t <- c_alpha # copy critical value for integral function 'func'

  if(test_type =="two-tailed"){
    fixed_power <- (1-pnorm(c_alpha - ncp)) + pnorm(-1*c_alpha - ncp)  ## Fixed power: Formula from Hedges & Pigott (2001)
    random_power <- 1 - (CDF(c_alpha, k, ncp, i2) - CDF(-c_alpha, k, ncp, i2))   ## Random power: Formula from Jackson & Turner (2017)
  } else {
    fixed_power <- 1 - pnorm(c_alpha - ncp)
    random_power <- 1 - (CDF(c_alpha, k, ncp, i2))
  }

  return(list(fixed_power = fixed_power,
              random_power = random_power))

}
