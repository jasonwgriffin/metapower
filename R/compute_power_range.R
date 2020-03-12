#' @export

compute_power_range <- function(effect_size, sample_size, k, model, test_type, p = .05){

  if(model == "fixed"){
    df <- tibble(es_v = rep(effect_size, times = 3*k),
                 n_v = rep(sample_size, times = 3*k),
                 k_v = seq(1:(3*k)))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v, model = model, test_type = test_type,p = p))
  } else if (model == "random"){
    df <- tibble(es_v = rep(effect_size,times = 3*3*k),
                 n_v = rep(sample_size, times = 3*3*k),
                 k_v = rep(seq(1:(3*k)), times = 3),
                 hg_v = rep(c("small","medium","large"), each = 3*k))
    df <- df %>%
      mutate(power = mapply(compute_power,df$es_v,df$n_v,df$k_v,df$hg_v,model = model, test_type = test_type,p = p)) # %>%
      mutate_at(vars(hg_v), factor)
  }
  return(df)
}

compute_power_range(.5,15,20,"random", "two-tailed")

effect_size = .5
sample_size = 15
k = 20
model = "random"
df <- tibble(es_v = rep(effect_size,times = 3*3*k),
             n_v = rep(sample_size, times = 3*3*k),
             k_v = rep(seq(1:(3*k)), times = 3),
             hg_v = rep(c("small","medium","large"), each = 3*k))

