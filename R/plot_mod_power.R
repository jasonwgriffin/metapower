#' Plot Power Curve for Categorical Moderators
#'
#' Plots power curves for categorical moderator in meta-analysis
#'
#' @export
#'
#' @param obj This should be an 'mod_power' object
#'
#' @return Power curves for moderator analysis under fixed and random effects models

plot_mod_power <- function(obj){

  if(class(obj) != "mod_power")
    stop("Object must be of class: mod_power")

  ## set aesthetic
  p_aes <- list(geom_hline(yintercept = .80, linetype = "dashed", alpha = .80),
                geom_line(size = 1.5),
                scale_x_continuous(limits = c(2,max(obj$mod_power_range$k_v)), breaks = c(seq(2,max(obj$mod_power_range$k_v),by = round(max(obj$mod_power_range$k_v)*.10,0)))),
                scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
                xlab("Number of Studies"),
                ylab("Power"),
                theme_bw(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

  ## random data for plot
  rand_dat <- obj$mod_power_range %>%
    dplyr::select(c("k_v", dplyr::starts_with("random"))) %>%
    tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power")

  rand_dat$power_type <- factor(rand_dat$power_type, levels = c("random_power_b_0","random_power_b_s", "random_power_b_m", "random_power_b_l"))
  #obj$mod_power_range$es_v <- as.factor(obj$power_range$es_v)


  # fixed plot
  fixed_plot <- ggplot(obj$mod_power_range, aes(x = .data$k_v, y = .data$fixed_power_b)) +
    p_aes +
    ggtitle("Fixed-Effects Model") +
    labs(linetype = "Effect Size")

  # random plot
  random_plot <- ggplot(rand_dat, aes(x = .data$k_v, y = .data$power, group = .data$power_type, color = .data$power_type)) +
    p_aes +
    ggtitle("Random-Effects Model") +
    scale_color_manual(name = "Heterogeneity",
                       labels = c(bquote(I^2 == "0%"),
                                  bquote(I^2 == "25%"),
                                  bquote(I^2 == "50%"),
                                  bquote(I^2 == "75%")),
                       values = rev(c("#993366", "#9900CC", "#9966FF", "#6699FF", "#33CCCC")))

  #arrange for plotting
  p <- cowplot::plot_grid(fixed_plot, random_plot, ncol = 1)

  return (p)

}
