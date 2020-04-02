
#' @importFrom stats pchisq
#' @importFrom stats qchisq

homogen_power <- function (effect_size, variance, sample_size, k, es_type, test_type, p, sd){

  df <- k-1
  c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
  weight = 1/round(variance,2)

  if(!is.null(sd)){
  ##fixed power
  fixed_lambda <- (k/variance)*(variance/k)*(sd^2)
  fixed_power <- (1 - pchisq(c_alpha,df,fixed_lambda,lower.tail = TRUE))
  }else{
  fixed_power <- NA
  }


  ##random power
  tau2_s <- (1/3)*variance
  tau2_m <- variance
  tau2_l <- 3*variance

  c <- k*weight- k*weight^2/(k*weight)

  u_s <- c*tau2_s+df
  u_m <- c*tau2_m+df
  u_l <- c*tau2_l+df

  sd_s <- 2*df + 4*c*(tau2_s^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_s^4
  sd_m <- 2*df + 4*c*(tau2_m^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_m^4
  sd_l <- 2*df + 4*c*(tau2_l^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2_l^4

  r_s = sd_s/(2*u_s)
  r_m = sd_m/(2*u_m)
  r_l = sd_l/(2*u_l)

  s_s = 2*u_s^2/sd_s
  s_m = 2*u_m^2/sd_m
  s_l = 2*u_l^2/sd_l

  homo_power <- data.frame(
    fixed_power = fixed_power,
    random_power_s = (1 - pchisq(c_alpha/r_s, s_s, ncp = 0, lower.tail = TRUE)),
    random_power_m = (1 - pchisq(c_alpha/r_m, s_m, ncp = 0, lower.tail = TRUE)),
    random_power_l = (1 - pchisq(c_alpha/r_l, s_l, ncp = 0, lower.tail = TRUE)))

  return(homo_power)
}

