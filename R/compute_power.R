
compute_power <- function(effect_size, variance, sample_size, k, es_type, model, hg, test_type, p){ # sd
  #sd <- NULL
  # Compute statistical Power
  if (model == "fixed"){
    lambda <- effect_size/(sqrt(variance/k))
  } else if (model == "random"){

      if(hg == "small"){
        tau2 <- (1/3)*variance
      } else if (hg == "medium"){
        tau2 <- variance
      } else if (hg == "large"){
        tau2 <- 3*variance
      }
      lambda <- effect_size/sqrt((tau2 + variance)/k)
    }

  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }

  power <- (1-pnorm(c_alpha - lambda)) + pnorm(-1*c_alpha - lambda)
  return(power)
}
