
compute_power <- function(effect_size, sample_size, k, hg, model, test_type, p, es_type){

  lambda <- 0
  # Compute statistical Power
  if (model == "fixed"){
    lambda <- (effect_size/sqrt(compute_variance(sample_size, effect_size)/k))
  } else if (model == "random"){

      if(hg == "small"){
        tau2 <- (1/3)*compute_variance(sample_size, effect_size)
      } else if (hg == "medium"){
        tau2 <- compute_variance(sample_size, effect_size)
      } else if (hg == "large"){
        tau2 <- 3*compute_variance(sample_size, effect_size)
      }
      lambda <- effect_size/sqrt((tau2 + compute_variance(sample_size, effect_size))/k)
    }

  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  b}

  power <- (1-pnorm(c_alpha - lambda)) + pnorm(-1*c_alpha - lambda)
  return(power)
}
