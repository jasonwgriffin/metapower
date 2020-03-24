compute_homogen_range <- function(effect_size, sample_size, k, es_type, model, hg, test_type, p, sd, variance){

  range_factor <- 5

if (es_type == "d"){
  if(model == "fixed"){

    homo_df <- tibble(SD = rep(seq(0,6), each = (k*range_factor-1)),
                      k_v = rep(seq(2,range_factor*k),times = 7),
                      es_v = effect_size,
                      n_v = sample_size,
                      Heterogenity = 0)
    homo_df <- homo_df %>%
      mutate(variance = mapply(compute_variance, sample_size, effect_size, es_type))

    homo_df <- homo_df %>%
      mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p, homo_df$SD))

     } else if (model == "random"){
       sd <- NA
       homo_df <- tibble(Heterogenity = rep(c("small","medium","large"), each = (k*range_factor-1)),
                       k_v = rep(seq(2,range_factor*k),times = 3),
                       es_v = effect_size,
                       n_v = sample_size,
                       SD = sd)
       homo_df <- homo_df %>%
         mutate(variance = mapply(compute_variance, sample_size, effect_size, es_type))

       homo_df <- homo_df %>%
         mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p))
   }
  return(homo_df)

  }else if (es_type == "Correlation"){

    if(model == "fixed"){

      homo_df <- tibble(SD = rep(seq(0,6), each = (k*range_factor-1)),
                        k_v = rep(seq(2,range_factor*k),times = 7),
                        es_v = effect_size,
                        n_v = sample_size,
                        Heterogenity = 0)
      homo_df <- homo_df %>%
        mutate(variance = mapply(compute_variance, sample_size, effect_size, es_type))

      homo_df <- homo_df %>%
        mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p, homo_df$SD))

    } else if (model == "random"){
      sd <- NA
      homo_df <- tibble(Heterogenity = rep(c("small","medium","large"), each = (k*range_factor-1)),
                        k_v = rep(seq(2,range_factor*k),times = 3),
                        es_v = effect_size,
                        n_v = sample_size,
                        SD = sd)
      homo_df <- homo_df %>%
        mutate(variance = mapply(compute_variance, sample_size, effect_size, es_type))

      homo_df <- homo_df %>%
        mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p))
    }
    return(homo_df)



  }else if (es_type == "OR"){
    if(model == "fixed"){

      homo_df <- tibble(SD = rep(seq(0,6), each = (k*range_factor-1)),
                        k_v = rep(seq(2,range_factor*k),times = 7),
                        es_v = effect_size,
                        n_v = sample_size,
                        Heterogenity = 0,
                        variance = variance)
      homo_df <- homo_df %>%
        mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p, homo_df$SD))

    } else if (model == "random"){
      sd <- NA
      homo_df <- tibble(Heterogenity = rep(c("small","medium","large"), each = (k*range_factor-1)),
                        k_v = rep(seq(2,range_factor*k),times = 3),
                        es_v = effect_size,
                        n_v = sample_size,
                        SD = sd,
                        variance = variance)

      homo_df <- homo_df %>%
        mutate(power = mapply(homogen_mpower, homo_df$es_v, homo_df$variance, homo_df$n_v, homo_df$k_v, es_type, model, homo_df$Heterogenity, test_type, p))
    }
    return(homo_df)

  }
}


