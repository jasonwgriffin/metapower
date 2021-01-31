
compute_subgroup_power <- function(n_groups, effect_sizes, variance, i2, overall_effect, study_size, k, c_alpha_b){

  df_b <- n_groups-1
  df_w <- k-n_groups

  ## between groups

  fixed_weight_c <- sum(rep(1/variance, k)) ## for subgroups do not divide by k
  fixed_lambda_b <- sum(fixed_weight_c*(effect_sizes-overall_effect)^2)

  ##within-groups
  #fixed_weight_w <-1/variance
  #fixed_var_w <- round(sqrt(1/sum(rep(fixed_weight_w,study_size/n_groups))),2)
  tau2 <- (i2*variance)/(1 - i2) # calculate tau2
  ## between groups
  random_weight_b <- sum(rep(1/(variance+tau2),k))#mod
  random_lambda_b <- sum(random_weight_b*(effect_sizes-overall_effect)^2)

  # power list
  subgroup_power_list <- data.frame(fixed_power_b = (1 - pchisq(c_alpha_b,df_b,fixed_lambda_b,lower.tail = TRUE)),
                                    random_power_b = (1 - pchisq(c_alpha_b,df_b,random_lambda_b,lower.tail = TRUE)))

}
