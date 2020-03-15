
compute_power_range <- function(effect_size, sample_size, k, model, test_type, p = .05){

  if(model == "fixed"){
    df <- tibble(es_v = rep(effect_size, times = 3*5*k-6),
                 n_v = rep(sample_size, times = 3*5*k-6),
                 k_v = rep(seq(2,5*k-1), times = 3),
                 `Effect Size` = rep(c((effect_size/2), effect_size, (effect_size*2)), each = 5*k-2))
    df <- df %>%
      mutate(power = mapply(compute_power,df$`Effect Size`,df$n_v,df$k_v, model = model, test_type = test_type,p = p))
  } else if (model == "random"){
    df <- tibble(es_v = rep(effect_size,times = 3*5*k-6),
                 n_v = rep(sample_size, times = 3*5*k-6),
                 k_v = rep(seq(2,5*k-1), times = 3),
                 Heterogenity = rep(c("small","medium","large"), each = 5*k-2))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v,df$Heterogenity,model = model, test_type = test_type,p = p))
  }
  return(df)
}

