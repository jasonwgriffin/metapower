#' @export

n_groups = 3
sample_size = 15
diff = .4
x1 = 0
x2 = .1
x3 =.55
effect_sizes = c(0,.1,.55)
effect_sizes = .9069
df = n_groups - 1
p = .05
k = 15
sd_v = c(1,1,4)

mod_power <- function(n_groups,
                      effect_sizes,
                      sample_size,
                      k,
                      model = c("fixed", "random"),
                      hg = c("small", "medium", "large"),
                      p = .05,
                      es_type = c("Correlation", "d", "OR"),
                      sd_v) {

  if(es_type == "d"){
    effect_sizes <- effect_sizes/sqrt(effect_sizes^2 + (sample_size + sample_size)^2/(sample_size*sample_size))
    effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes))
  }else if(es_type == "OR") {
    effect_sizes = effect_sizes*(sqrt(3)/pi)
    effect_sizes <- effect_sizes/sqrt(effect_sizes^2 + (sample_size + sample_size)^2/(sample_size*sample_size))
    effect_sizes <- 0.5*log((1+effect_sizes)/(1-effect_sizes))
  }

  effect_sizes <- effect_sizes
  overall_effect_size <- mean(effect_sizes)
  df_b <- n_groups-1
  df_w <- k-n_groups
  c_alpha_b <- qchisq(1-p,df_b,0, lower.tail = TRUE)
  c_alpha_w <- qchisq(1-p, df_w,0,lower.tail = TRUE)

  if(model == "fixed") {
    hg = NA
    ## between groups
    weight <- sum(rep(sample_size-n_groups,sample_size/n_groups))
    #weight <- sum(rep(compute_variance(sample_size,overall_effect_size)/k,k/n_groups))
    lambda_b <- sum(weight*(effect_sizes-overall_effect_size)^2)
    power_b = 1 - pchisq(c_alpha_b,df_b,lambda_b,lower.tail = TRUE)
    ##within-groups
    weight_w <-1/(1/(sample_size-n_groups))
    var_w <- round(sqrt(1/sum(rep(1/(1/(sample_size-3)),sample_size/n_groups))),2)
    lambda_w <- sum(rep(weight_w*(sd_v*var_w)^2, sample_size/n_groups))
    power_w <- 1 - pchisq(c_alpha_w,df_w,lambda_w,lower.tail = TRUE)
   } else if(model =="random") {
     if(hg == "small"){
       tau2 <- (1/3)*(1/(sample_size - 3))
     } else if (hg == "medium"){
       tau2 <- (1)*(1/(sample_size - 3))
     } else if (hg == "large"){
       tau2 <- (3)*(1/(sample_size - 3))
     }
     ## between groups
     weight_b <- 1/sum(rep(1/(1/(sample_size-3)+tau2),sample_size/n_groups))
     lambda_b <- sum(weight_b*(effect_sizes-overall_effect_size)^2)
     power_b = 1 - pchisq(c_alpha_b,df_b,lambda_b,lower.tail = TRUE)
     power_w <- NA
     }

  power_list <- list(power_b = power_b,
                     power_w = power_w,
                     effect_sizes = effect_sizes,
                     sample_size = sample_size,
                     k = k,
                     model = model,
                     hg = hg,
                     n_groups = n_groups,
                     es_type = es_type,
                     df_b = df_b,
                     df_w = df_w)
  attr(power_list, "class") <- "mod_power"
  return(power_list)
}


mod1 <- mod_power(3, c(0,.1,.55), sample_size = 15, k = 15, model = "random", hg = "small",p = .05, es_type = "Correlation",
          sd_v = c(1,1,4))
mod1
