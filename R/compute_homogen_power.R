compute_homogen_power <- function(k, effect_size, variance, c_alpha){
  df <- k-1
  weight = 1/round(variance,2)

  ##fixed power ncp
  fixed_lambda_sd1 <- (k/variance)*(variance/k)*(1^2)
  fixed_lambda_sd2 <- (k/variance)*(variance/k)*(2^2)
  fixed_lambda_sd3 <- (k/variance)*(variance/k)*(3^2)
  fixed_lambda_sd4 <- (k/variance)*(variance/k)*(4^2)
  fixed_lambda_sd5 <- (k/variance)*(variance/k)*(5^2)

  ##random power
  tau2_0 <- 0
  tau2_s <- (1/3)*variance  ## i2 = .25
  tau2_m <- variance  ## i2 = .50
  tau2_l <- 3*variance  ## i2 = .75

  c <- k*weight- k*weight^2/(k*weight)

  u_0 <- c*tau2_0+df
  u_s <- c*tau2_s+df
  u_m <- c*tau2_m+df
  u_l <- c*tau2_l+df

  sd_0 <- 2*df + 4*c*(tau2_0^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_0^4
  sd_s <- 2*df + 4*c*(tau2_s^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_s^4
  sd_m <- 2*df + 4*c*(tau2_m^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_m^4
  sd_l <- 2*df + 4*c*(tau2_l^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_l^4

  r_0 = sd_0/(2*u_0)
  r_s = sd_s/(2*u_s)
  r_m = sd_m/(2*u_m)
  r_l = sd_l/(2*u_l)

  s_0 = 2*u_0^2/sd_0
  s_s = 2*u_s^2/sd_s
  s_m = 2*u_m^2/sd_m
  s_l = 2*u_l^2/sd_l

  homogen_power <- data.frame(
    fixed_power_sd1 = (1 - pchisq(c_alpha,df,fixed_lambda_sd1,lower.tail = TRUE)),
    fixed_power_sd2 = (1 - pchisq(c_alpha,df,fixed_lambda_sd2,lower.tail = TRUE)),
    fixed_power_sd3 = (1 - pchisq(c_alpha,df,fixed_lambda_sd3,lower.tail = TRUE)),
    fixed_power_sd4 = (1 - pchisq(c_alpha,df,fixed_lambda_sd4,lower.tail = TRUE)),
    fixed_power_sd5 = (1 - pchisq(c_alpha,df,fixed_lambda_sd5,lower.tail = TRUE)),
    random_power_0 = (1 - pchisq(c_alpha/r_0, s_0, ncp = 0, lower.tail = TRUE)),
    random_power_s = (1 - pchisq(c_alpha/r_s, s_s, ncp = 0, lower.tail = TRUE)),
    random_power_m = (1 - pchisq(c_alpha/r_m, s_m, ncp = 0, lower.tail = TRUE)),
    random_power_l = (1 - pchisq(c_alpha/r_l, s_l, ncp = 0, lower.tail = TRUE)))

  return(homogen_power)

}
