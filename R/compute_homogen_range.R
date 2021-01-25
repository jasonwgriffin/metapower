compute_homogen_range <- function(homogen_power_range_df){

  homogen_power_range_df <- cbind.data.frame(homogen_power_range_df, as.data.frame((t(mapply(compute_homogen_power, homogen_power_range_df$k, homogen_power_range_df$es_v, homogen_power_range_df$variance, homogen_power_range_df$i2, homogen_power_range_df$c_alpha)))) %>%
                                               dplyr::mutate_all(as.numeric))

    return(homogen_power_range_df)

  }
