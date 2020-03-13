
compute_power_range <- function(effect_size, sample_size, k, model, test_type, p = .05){

  if(model == "fixed"){
    df <- tibble(es_v = rep(effect_size, times = 10*k-2),
                 n_v = rep(sample_size, times = 10*k-2),
                 k_v = seq(2,10*k-1))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v, model = model, test_type = test_type,p = p))
  } else if (model == "random"){
    df <- tibble(es_v = rep(effect_size,times = 3*10*k-6),
                 n_v = rep(sample_size, times = 3*10*k-6),
                 k_v = rep(seq(2,10*k-1), times = 3),
                 hg_v = rep(c("small","medium","large"), each = 10*k-2))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v,df$hg_v,model = model, test_type = test_type,p = p)) %>%
      mutate_at(vars(hg_v), factor)
  }
  return(df)
}

