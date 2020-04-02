compute_homogen_range <- function(effect_size, sample_size, k, es_type, test_type, p, sd, con_table){

  if(missing(con_table))
    con_table = NA

  range_factor <- 5

  if(es_type == "d" | es_type == "Correlation"){
    homo_range <- data.frame(SD = rep(seq(0,6), each = (k*range_factor-1)),
                      k_v = rep(seq(2,range_factor*k),times = 7),
                      es_v = effect_size,
                      n_v = sample_size,
                      variance = mapply(compute_variance, sample_size, effect_size, es_type))
    homo_range <- cbind.data.frame(homo_range, as.data.frame((t(mapply(homogen_power, homo_range$es_v, homo_range$variance, homo_range$n_v, homo_range$k_v, es_type, test_type, p, homo_range$SD)))) %>%
                                     dplyr::mutate_all(as.numeric))
    return(homo_range)

    }else if (es_type =="OR"){

    variance <- compute_variance(sample_size, effect_size, es_type, con_table)
    homo_range <- data.frame(SD = rep(seq(0,6), each = (k*range_factor-1)),
                         k_v = rep(seq(2,range_factor*k),times = 7),
                         es_v = effect_size,
                         n_v = sample_size,
                         variance = variance)
    homo_range <- cbind.data.frame(homo_range, as.data.frame((t(mapply(homogen_power, homo_range$es_v, homo_range$variance, homo_range$n_v, homo_range$k_v, es_type, test_type, p, homo_range$SD)))) %>%
                                     dplyr::mutate_all(as.numeric))
    return(homo_range)
  }
}



