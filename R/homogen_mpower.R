
#' @importFrom stats pchisq
#' @importFrom stats qchisq

homogen_mpower <- function (effect_size, sample_size, k, hg, model, test_type, p, sd){

  df <- k-1
  c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
  weight = 1/round(compute_variance(sample_size, effect_size),2)

  if(model == "fixed"){
    lambda <- (k/compute_variance(sample_size, effect_size))*(compute_variance(sample_size, effect_size)/k)*(sd^2)
    #weight = 1/round(compute_variance(sample_size, effect_size),2)
    power <- (1 - pchisq(c_alpha,df,lambda,lower.tail = TRUE))
  } else if (model =="random"){
    if(hg == "small"){
      tau2 <- (1/3)*compute_variance(sample_size, effect_size)
    } else if (hg == "medium"){
      tau2 <- compute_variance(sample_size, effect_size)
    } else if (hg == "large"){
      tau2 <- 3*compute_variance(sample_size, effect_size)
    }
    c <- k*weight- k*weight^2/(k*weight)
    u_q <- c*tau2+df
    sd_q <- 2*df + 4*c*(tau2^2) + 2*(k*(weight^2) - 2*(k*(weight^3)/(k*weight)) + k*(weight^2)^2/(k*(weight^2))) * tau2^4
    r = sd_q/(2*u_q)
    s = 2*u_q^2/sd_q
    power <- (1 - pchisq(c_alpha/r,s,ncp = 0, lower.tail = TRUE))
  }
  return(power)
}
