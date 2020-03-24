
compute_power_range <- function(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance){

  range_factor <- 5

  if(es_type == "d"){
    if(model == "fixed"){
    df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                 EffectSize = rep(c((effect_size/2), effect_size, (effect_size*2)), each = range_factor*k-1),
                 es_v = effect_size,
                 n_v = sample_size,
                 Heterogenity = 0)
    df <- df %>%
      mutate(variance = mapply(compute_variance, sample_size, df$EffectSize, es_type))
    df <- df %>%
      mutate(power = mapply(compute_power, df$EffectSize, df$variance, df$n_v, df$k_v, es_type, model, df$Heterogenity, test_type, p))

    } else if (model == "random"){
    df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                 es_v = effect_size,
                 n_v = sample_size,
                 Heterogenity = rep(c("small","medium","large"), each = range_factor*k-1))
    df <- df %>%
      mutate(variance = mapply(compute_variance, sample_size, df$es_v, es_type))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v, df$variance, df$n_v,df$k_v, es_type, model, df$Heterogenity, test_type, p))
  }
  return(df)

    }else if (es_type == "Correlation"){
      if(effect_size*2 >= 2.64){
        max = 2.64
      }else{
        max = effect_size*2
      }
      if(model == "fixed"){
        df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                     EffectSize = rep(c((effect_size/2), effect_size, max), each = range_factor*k-1),
                     es_v = effect_size,
                     n_v = sample_size,
                     Heterogenity = 0)
        df <- df %>%
          mutate(variance = mapply(compute_variance, sample_size, df$EffectSize, es_type))
        df <- df %>%
          mutate(power = mapply(compute_power, df$EffectSize, df$variance, df$n_v, df$k_v, es_type, model, df$Heterogenity, test_type, p))

      } else if (model == "random"){
        df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                     es_v = effect_size,
                     n_v = sample_size,
                     Heterogenity = rep(c("small","medium","large"), each = range_factor*k-1))
        df <- df %>%
          mutate(variance = mapply(compute_variance, sample_size, df$es_v, es_type))
        df <- df %>%
          mutate(power = mapply(compute_power,df$es_v, df$variance, df$n_v,df$k_v, es_type, model, df$Heterogenity, test_type, p))
      }
      return(df)



}else if (es_type =="OR"){
  if(model == "fixed"){
    df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                 EffectSize = rep(c((effect_size/2), effect_size, (effect_size*2)), each = range_factor*k-1),
                 es_v = effect_size,
                 n_v = sample_size,
                 Heterogenity = 0,
                 variance = variance)
    df <- df %>%
      mutate(power = mapply(compute_power, df$EffectSize, df$variance, df$n_v, df$k_v, es_type, model, df$Heterogenity, test_type, p))

  } else if (model == "random"){
    df <- tibble(k_v = rep(seq(2,range_factor*k), times = 3),
                 es_v = effect_size,
                 n_v = sample_size,
                 Heterogenity = rep(c("small","medium","large"), each = range_factor*k-1),
                 variance = variance)

    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v, df$variance, df$n_v,df$k_v, es_type, model, df$Heterogenity, test_type, p))
  }
  return(df)

  }
}



