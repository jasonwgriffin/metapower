#' Plot Power Curve for Meta-analysis
#'
#' Plots power curves for fixed effects models with various effect size magnitudes. Also plots power curves for various levels
#' of heterogeneity (e.g., i2 = 75%)
#'
#' @export
#'
#' @param obj This should be an "mpower" object
#'
#' @return Power curve plot for the user specified input parameters

plot_mpower <- function(obj){

  if(class(obj) != "mpower")
    stop("Object must be of class: mpower")

  ## set aesthetic
  p_aes <- list(geom_hline(yintercept = .80, linetype = "dashed", alpha = .80),
                geom_line(size = 1.5),
                scale_x_continuous(limits = c(2,max(obj$power_range$k_v)), breaks = c(seq(2,max(obj$power_range$k_v),by = round(max(obj$power_range$k_v)*.10,0)))),
                scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
                xlab("Number of Studies"),
                ylab("Power"),
                theme_bw(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

  ## random data for plot
  rand_dat <- obj$power_range %>%
    dplyr::filter(obj$power_range$es_v == obj$effect_size) %>%
    dplyr::select(c("k_v", dplyr::starts_with("random"))) %>%
    tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power")
  obj$power_range$es_v <- as.factor(obj$power_range$es_v)


  # fixed plot
  fixed_plot <- ggplot(obj$power_range, aes(x = .data$k_v, y = .data$fixed_power, color = .data$es_v)) +
      p_aes +
      ggtitle("Fixed-Effects Model") +
      labs(color = "Effect Size")

  # random plot
  random_plot <- ggplot(rand_dat, aes(x = .data$k_v, y = .data$power)) +
    p_aes +
    geom_point(aes(x = obj$k, y = obj$power$random_power), size = 4, shape = 21, fill = "white", stroke = 1) +
    ggtitle(paste0("Random-Effects Model (I^2 = ", round(obj$i2*100,2),"%)"))

  #arrange for plotting
  p <- cowplot::plot_grid(fixed_plot, random_plot, ncol = 1)

  return (p)

}
