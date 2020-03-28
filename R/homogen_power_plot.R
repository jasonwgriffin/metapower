#' Plot power curve
#'
#' @export
#'
#' @param obj This should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters

homogen_power_plot <- function(x444){

  p_aes <- list(geom_line(size = 1),

                scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
                #geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3),
                xlab("Standard deviation"),
                ylab("Power"),
                theme_classic(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))


      fix_dat <- x444$homo_range %>% dplyr::filter(x444$homo_range$k_v == round(x444$k/2) |
                                                             x444$homo_range$k_v == x444$k |
                                                             x444$homo_range$k_v == x444$k*2); fix_dat$k_v <- as.factor(fix_dat$k_v)

      rand_dat <- x444$df %>%
        dplyr::filter(es_v == effect_size) %>%
        dplyr::select(k_v, starts_with("rand")) %>%
        tidyr::pivot_longer(-k_v, names_to = "power_type", values_to = "power")

      fixed_plot <- ggplot(fix_dat, aes(x = SD, y = fixed_power, linetype = k_v)) +
        geom_line(size = 1)+
        scale_x_continuous(limits = c(0, 6), breaks = c(seq(1:6))) +
        p_aes

      random_plot <- ggplot(rand_dat, aes(x = k_v, y = power, color = power_type)) +
        geom_line(size = 1)+
        scale_x_continuous(limits = c(2,max(x444$homo_range$k_v)), breaks = c(seq(2,max(x444$homo_range$k_v),by = round(max(x444$homo_range$k_v)*.10,0)))) +
        p_aes

      p <- cowplot::plot_grid(fixed_plot,random_plot,ncol=1)

  return (p)
}


