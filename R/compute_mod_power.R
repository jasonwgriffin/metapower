
compute_mod_power <- function(n_groups, effect_sizes, variance, overall_effect, sample_size, k, c_alpha_b){

  df_b <- n_groups-1
  df_w <- k-n_groups

  ## between groups

  #fixed_weight_c <- sum(rep(1/variance, k/n_groups))
  fixed_weight_c <- sum(rep(1/variance, k/n_groups))
  fixed_lambda_b <- sum(fixed_weight_c*(effect_sizes-overall_effect)^2)
  fixed_power_b <- 1 - pchisq(c_alpha_b,df_b,fixed_lambda_b,lower.tail = TRUE)

  ##within-groups
  #fixed_weight_w <-1/variance
  #fixed_var_w <- round(sqrt(1/sum(rep(fixed_weight_w,sample_size/n_groups))),2)

  tau2_0 <- 0
  tau2_s <- (1/3)*variance # i2 = .25
  tau2_m <- (1)*variance  #i2 = .50
  tau2_l <- (3)*variance  # i2 = .75

  ## between groups
  random_weight_b_0 <- sum(rep(1/(variance+tau2_0),k/n_groups))
  random_weight_b_s <- sum(rep(1/(variance+tau2_s),k/n_groups))
  random_weight_b_m <- sum(rep(1/(variance+tau2_m),k/n_groups))
  random_weight_b_l <- sum(rep(1/(variance+tau2_l),k/n_groups))

  random_lambda_b_0 <- sum(random_weight_b_0*(effect_sizes-overall_effect)^2)
  random_lambda_b_s <- sum(random_weight_b_s*(effect_sizes-overall_effect)^2)
  random_lambda_b_m <- sum(random_weight_b_m*(effect_sizes-overall_effect)^2)
  random_lambda_b_l <- sum(random_weight_b_l*(effect_sizes-overall_effect)^2)

  random_power_b_0 <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_0,lower.tail = TRUE)
  random_power_b_s <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_s,lower.tail = TRUE)
  random_power_b_m <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_m,lower.tail = TRUE)
  random_power_b_l <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_l,lower.tail = TRUE)

  mod_power_list <- data.frame(fixed_power_b = fixed_power_b,
                           random_power_b_0 = random_power_b_0,
                           random_power_b_s = random_power_b_s,
                           random_power_b_m = random_power_b_m,
                           random_power_b_l = random_power_b_l,
                           fixed_lambda_b = fixed_lambda_b)
}
