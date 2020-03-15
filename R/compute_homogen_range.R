compute_homogen_range <- function(effect_size, sample_size, k, model, test_type, p, sd){

  range_factor <- 5

  if(model == "fixed"){

    homo_df <- tibble(SD = rep(seq(0,6), each = (k*range_factor-1)),
                      k_v = rep(seq(2,range_factor*k),times = 7),
                      es_v = effect_size,
                      n_v = sample_size)

    homo_df <- homo_df %>%
      mutate(power = mapply(homogen_mpower,
                            homo_df$es_v,
                            homo_df$n_v,
                            homo_df$k_v,
                            model = model,
                            test_type = test_type,
                            p = p,
                            sd = homo_df$SD))

     } else if (model == "random"){

       homo_df <- tibble(Heterogenity = rep(c("small","medium","large"), each = (k*range_factor-1)),
                       k_v = rep(seq(2,range_factor*k),times = 3),
                       es_v = effect_size,
                       n_v = sample_size,
                       SD = sd)
       homo_df <- homo_df %>%
       mutate(power = mapply(homogen_mpower,homo_df$es_v,homo_df$n_v,homo_df$k_v,homo_df$Heterogenity, sd = homo_df$SD, model = model, test_type = test_type,p = p))
   }
  return(homo_df)
}


