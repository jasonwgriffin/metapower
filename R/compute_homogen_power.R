compute_homogen_power <- function(k, effect_size, variance,i2, c_alpha){
  df <- k-1
  weight = 1/round(variance,2)

  ##fixed power ncp
  fixed_lambda_sd1 <- (k/variance)*(variance/k)*(1^2)
  fixed_lambda_sd2 <- (k/variance)*(variance/k)*(2^2)
  fixed_lambda_sd3 <- (k/variance)*(variance/k)*(3^2)
  fixed_lambda_sd4 <- (k/variance)*(variance/k)*(4^2)
  fixed_lambda_sd5 <- (k/variance)*(variance/k)*(5^2)

  ##random power
  tau2 <- (i2*variance)/(1 - i2) # calculate tau2

  c <- sum(k*weight) - sum(k*weight^2)/sum(k*weight) ## sum?
  u <- c*tau2+df
  sd <- 2*df + 4*(weight-(weight^2/weight))*tau2 + 2*(weight^2 - 2*((weight^3)/(weight)) + (weight^2)^2/((weight^2))) * tau2^2 # correct formula

  r = sd/(2*u)
  s = 2*u^2/sd

  homogen_power <- data.frame(
    fixed_power_sd1 = (1 - pchisq(c_alpha,df,fixed_lambda_sd1,lower.tail = TRUE)),
    fixed_power_sd2 = (1 - pchisq(c_alpha,df,fixed_lambda_sd2,lower.tail = TRUE)),
    fixed_power_sd3 = (1 - pchisq(c_alpha,df,fixed_lambda_sd3,lower.tail = TRUE)),
    fixed_power_sd4 = (1 - pchisq(c_alpha,df,fixed_lambda_sd4,lower.tail = TRUE)),
    fixed_power_sd5 = (1 - pchisq(c_alpha,df,fixed_lambda_sd5,lower.tail = TRUE)),
    random_power = (1 - pchisq(c_alpha/r, s, ncp = 0, lower.tail = TRUE)))

  return(homogen_power)

}
