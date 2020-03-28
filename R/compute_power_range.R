
compute_power_range <- function(effect_size, sample_size, k, es_type, test_type, p){

  range_factor <- 5

  if(es_type == "d" | es_type == "OR"){
    es_v = rep(c((effect_size/2), effect_size, (effect_size*2)), each = range_factor*k-1)
  }else if (es_type == "Correlation"){
    if(effect_size*2 >= 2.64){ #2.64 is the maximum correlation value in fisher z units
      max = 2.64
    }else{
      max = effect_size*2
    }
    es_v = rep(c((effect_size/2), effect_size, max), each = range_factor*k-1)
  }

  df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                 es_v = es_v,
                 effect_size = effect_size,
                 n_v = sample_size,
                 variance = mapply(compute_variance, sample_size, es_v, es_type))
  df <- cbind.data.frame(df, as.data.frame((t(mapply(compute_power, df$es_v, df$variance, df$n_v, df$k_v, es_type, test_type, p)))) %>%
                           mutate_all(as.numeric))
  return(df)
}
library(tidyr)
