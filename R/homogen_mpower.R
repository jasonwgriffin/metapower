
#' @importFrom stats pchisq
#' @importFrom stats qchisq

homogen_mpower <- function (effect_size, variance, sample_size, k, es_type, model, hg, test_type, p, sd){

  df <- k-1
  c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
  weight = 1/round(variance,2)

  if(model == "fixed"){
    lambda <- (k/variance)*(variance/k)*(sd^2)
    #weight = 1/round(compute_variance(sample_size, effect_size),2)
    power <- (1 - pchisq(c_alpha,df,lambda,lower.tail = TRUE))
  } else if (model =="random"){
    #sd <- NA
    if(hg == "small"){
      tau2 <- (1/3)*variance
    } else if (hg == "medium"){
      tau2 <- variance
    } else if (hg == "large"){
      tau2 <- 3*variance
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

