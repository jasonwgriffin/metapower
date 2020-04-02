
compute_mod_power <- function(n_groups, effect_sizes, sample_size, k, es_type, test_type = "two-tailed", p = .05, sd_within, con_table){

  overall_effect_diff <- mean(effect_sizes) # find overall mean

  df_b <- n_groups-1
  df_w <- k-n_groups

  if(test_type == "two-tailed"){
    c_alpha_b <- qchisq(1-(p/2),df_b,0, lower.tail = TRUE)
    c_alpha_w <- qchisq(1-(p/2), df_w,0,lower.tail = TRUE)

    }else if(test_type == "one-tailed"){
      c_alpha_b <- qchisq(1-p,df_b,0, lower.tail = TRUE)
      c_alpha_w <- qchisq(1-p, df_w,0,lower.tail = TRUE)

      }

  variance <- compute_variance(sample_size, overall_effect_diff, es_type, con_table)

  ## between groups
  fixed_weight_c <- sum(rep(1/variance,sample_size/n_groups))
  fixed_lambda_b <- sum(fixed_weight_c*(effect_sizes-overall_effect_diff)^2)
  fixed_power_b <- 1 - pchisq(c_alpha_b,df_b,fixed_lambda_b,lower.tail = TRUE)
  ##within-groups
  fixed_weight_w <-1/variance
  fixed_var_w <- round(sqrt(1/sum(rep(fixed_weight_w,sample_size/n_groups))),2)

  if(is.null(sd_within)){
    sd_within <- NA
    fixed_lambda_w <- NA
    fixed_power_w <- NA
    } else {
      fixed_lambda_w <- sum(rep(fixed_weight_w*(sd_within*fixed_var_w)^2, sample_size/n_groups))
      fixed_power_w <- 1 - pchisq(c_alpha_w,df_w,fixed_lambda_w,lower.tail = TRUE)
      }

  tau2_s <- (1/3)*variance
  tau2_m <- (1)*variance
  tau2_l <- (3)*variance

  ## between groups
  random_weight_b_s <- 1/sum(rep(1/(variance+tau2_s),sample_size/n_groups))
  random_weight_b_m <- 1/sum(rep(1/(variance+tau2_m),sample_size/n_groups))
  random_weight_b_l <- 1/sum(rep(1/(variance+tau2_l),sample_size/n_groups))

  random_lambda_b_s <- sum(random_weight_b_s*(effect_sizes-overall_effect_diff)^2)
  random_lambda_b_m <- sum(random_weight_b_m*(effect_sizes-overall_effect_diff)^2)
  random_lambda_b_l <- sum(random_weight_b_l*(effect_sizes-overall_effect_diff)^2)

  random_power_b_s <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_s,lower.tail = TRUE)
  random_power_b_m <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_m,lower.tail = TRUE)
  random_power_b_l <- 1 - pchisq(c_alpha_b,df_b,random_lambda_b_l,lower.tail = TRUE)

  random_lambda_w <- NA
  random_power_w <- NA

  mod_power_list <- data.frame(fixed_power_b = fixed_power_b,
                           fixed_power_w = fixed_power_w,
                           random_power_b_s = random_power_b_s,
                           random_power_b_m = random_power_b_m,
                           random_power_b_l = random_power_b_l)
}
