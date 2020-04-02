compute_power <- function(effect_size, variance, sample_size, k, es_type, test_type, p){
  # noncentraility parameter
    fixed_lambda <- effect_size/(sqrt(variance/k))
    random_lambda_s <- effect_size/sqrt(((1/3)*variance + variance)/k) #small
    random_lambda_m <- effect_size/sqrt(((1)*variance + variance)/k) #moderate
    random_lambda_l <- effect_size/sqrt(((3)*variance + variance)/k) #large
  # critical value

  if(test_type == "two-tailed"){
    c_alpha <- qnorm(1-(p/2))
  } else if (test_type =="one-tailed") {
    c_alpha <- qnorm(1-(p))
  }

  main_effect_power <- data.frame(
    fixed_power    = (1-pnorm(c_alpha - fixed_lambda)) + pnorm(-1*c_alpha - fixed_lambda),
    random_power_s = (1-pnorm(c_alpha - random_lambda_s)) + pnorm(-1*c_alpha - random_lambda_s),
    random_power_m = (1-pnorm(c_alpha - random_lambda_m )) + pnorm(-1*c_alpha - random_lambda_m),
    random_power_l = (1-pnorm(c_alpha - random_lambda_l)) + pnorm(-1*c_alpha - random_lambda_l))

  return(main_effect_power)
}
