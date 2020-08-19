fixed_power <- function(effect_size, variance, k, test_type, p){
  # noncentrality parameter
    fixed_lambda <- effect_size/(sqrt(variance/k))
  # critical value
  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }
  fixed_power = (1-pnorm(c_alpha - fixed_lambda)) + pnorm(-1*c_alpha - fixed_lambda)

  return(fixed_power)
}
