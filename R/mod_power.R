#' @export


n_groups = 3
sample_size = 15
diff = .4
x1 = 0
x2 = .1
x3 =.55
effect_sizes = c(0,.1,.55)
df = n_groups - 1
p = .05
k = 15
mod_power <- function(n_groups, effect_sizes, sample_size,k, model = c("fixed", "random"),p = .05) {

  effect_sizes <- effect_sizes
  overall_effect_size <- mean(effect_sizes)
  df <- n_groups-1
  c_alpha <- qchisq(1-p,df,0, lower.tail = TRUE)
  if(model == "fixed") {
    #weight <- sum(rep(sample_size-n_groups,sample_size/n_groups)) fishers z
    weight <- sum(rep(compute_variance(sample_size,overall_effect_size)/k,sample_size/n_groups))
    lambda <- sum(weight*(effect_sizes-overall_effect_size)^2)
   } else if(model =="random") {
    v = d
    }
  power <- (1 - pchisq(c_alpha,df,lambda,lower.tail = TRUE))
  return(power)
}


mod_power(3,c(0,.1,.55),sample_size = 15, k = 15, model = "fixed", p = .05)
