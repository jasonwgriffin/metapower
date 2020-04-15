#' Plot Power Curve for Test of Homogeneity
#'
#' @export
#'
#' @param obj should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters

homogen_power_plot <- function(obj){

  p_aes <- list(geom_line(size = 1),
                scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
                #geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3),
                xlab("Standard deviation"),
                ylab("Power"),
                theme_classic(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

      fix_dat <- obj$homo_range %>% dplyr::filter(obj$homo_range$k_v == round(obj$k/2) | obj$homo_range$k_v == obj$k | obj$homo_range$k_v == obj$k*2);
      fix_dat$k_v <- as.factor(fix_dat$k_v)

      rand_dat <- obj$homo_range %>%
        dplyr::filter(obj$homo_range$es_v == obj$effect_size) %>%
        dplyr::select("k_v", "random_power_s", "random_power_m", "random_power_l") %>%
        tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power")

      fixed_plot <- ggplot(fix_dat, aes(x = .data$SD, y = .data$fixed_power, linetype = .data$k_v)) +
        geom_line(size = 1)+
        scale_x_continuous(limits = c(0, 6), breaks = c(seq(1:6))) +
        p_aes

      random_plot <- ggplot(rand_dat, aes(x = .data$k_v, y = .data$power, color = .data$power_type)) +
        geom_line(size = 1)+
        scale_x_continuous(limits = c(2,max(obj$homo_range$k_v)), breaks = c(seq(2,max(obj$homo_range$k_v),by = round(max(obj$homo_range$k_v)*.10,0)))) +
        p_aes +
        scale_color_manual(name = "Heterogeneity",
                           labels = c("Large", "Moderate", "Low"),
                           values = c("red","blue","green"))

      p <- cowplot::plot_grid(fixed_plot,random_plot,ncol=1)

  return (p)
}


