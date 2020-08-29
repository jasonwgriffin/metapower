#' Plot Power Curve for Test of Homogeneity
#'
#' Plots power curves for the test of homogeneity for different levels of within-study variation for fixed effects models.
#' For random-effects models, power curves are plotted for various levels of heterogeneity.
#'
#' @export
#'
#' @param obj should be an "homogen_power" object
#'
#' @return Power curve plot for the user specified input parameters

plot_homogen_power <- function(obj){

  p_aes <- list(geom_hline(yintercept = .80, linetype = "dashed", alpha = .80),
                geom_line(size = 1.5),
                scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
                ylab("Power"),
                theme_bw(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

      fix_dat <- obj$homogen_power_range %>%
        dplyr::select(c("k_v",dplyr::starts_with("fixed"))) %>%
        tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power")

      rand_dat <- obj$homogen_power_range %>%
        dplyr::filter(obj$homogen_power_range$es_v == obj$effect_size) %>%
        dplyr::select("k_v", "random_power_0", "random_power_s", "random_power_m", "random_power_l") %>%
        tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power")
      rand_dat$power_type <- factor(rand_dat$power_type, levels = c("random_power_0", "random_power_s", "random_power_m", "random_power_l"))

      fixed_plot <- ggplot(fix_dat, aes(x = .data$k_v, y = .data$power, color = .data$power_type)) +
        geom_line(size = 1) +
        ggtitle("Fixed-Effects Model") +
        xlab("Number of Studies") + p_aes +
        scale_color_manual(name = "Standard Deviation \n between Studies",
                           labels = c("SD = 1",
                                      "SD = 2",
                                      "SD = 3",
                                      "SD = 4",
                                      "SD = 5"),
                           values = rev(c("#993366", "#9900CC", "#9966FF", "#6699FF", "#33CCCC")))

      random_plot <- ggplot(rand_dat, aes(x = .data$k_v, y = .data$power, color = .data$power_type)) +
        geom_line(size = 1)+
        #scale_x_continuous(limits = c(2,max(obj$homo_range$k_v)), breaks = c(seq(2,max(obj$homo_range$k_v),by = round(max(obj$homo_range$k_v)*.10,0)))) +
        p_aes +
        ggtitle("Random-Effects Model") +
        xlab("Number of Studies") +
        scale_color_manual(name = "Heterogeneity",
                           labels = c(bquote(I^2 == "0%"),
                                      bquote(I^2 == "25%"),
                                      bquote(I^2 == "50%"),
                                      bquote(I^2 == "75%")),
                           values = rev(c("#993366", "#9900CC", "#9966FF", "#6699FF")))

      p <- cowplot::plot_grid(fixed_plot,random_plot,ncol=1)

  return (p)
}
