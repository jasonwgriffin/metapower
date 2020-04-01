#' Plot power curve
#'
#' @export
#'
#' @param obj This should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters
#'

power_plot <- function(obj){

  ## set aesthetic
  p_aes <- list(geom_line(size = 1),
    scale_x_continuous(limits = c(2,max(obj$df$k_v)), breaks = c(seq(2,max(obj$df$k_v),by = round(max(obj$df$k_v)*.10,0)))),
    scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)),
    #geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3),
    xlab("Number of Studies"),
    ylab("Power"),
    theme_classic(),
    theme(legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

  if(class(obj) == "mpower"){

    rand_dat <- obj$df %>%
      dplyr::filter(obj$df$es_v == obj$effect_size) %>%
      dplyr::select(obj$df$k_v, dplyr::starts_with("rand")) %>%
      tidyr::pivot_longer(-obj$df$k_v, names_to = "power_type", values_to = "power")

    obj$df$es_v <- as.factor(obj$df$es_v)

    fixed_plot <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$fixed_power, linetype = obj$df$es_v)) +
      p_aes +
      ggtitle("Fixed-Effects Model") +
      labs(linetype = "Effect Size")

    random_plot <- ggplot(rand_dat, aes(x = rand_dat$k_v, y = rand_dat$power, group = rand_dat$power_type, color = rand_dat$power_type)) +
      p_aes +
      ggtitle("Random-Effects Model") +
      scale_color_manual(name = "Heterogenity",
                         labels = c("Large", "Moderate", "Low"),
                         values = c("red","blue","green"))

    p <- cowplot::plot_grid(fixed_plot, random_plot, ncol = 1)

  }else if (class(obj) == "modpower"){

  rand_dat <- obj$df %>%
    dplyr::filter(obj$df$es_v == obj$effect_size) %>%
    dplyr::select(obj$df$k_v, dplyr::starts_with("rand")) %>%
    tidyr::pivot_longer(-obj$df$k_v, names_to = "power_type", values_to = "power")

  obj$df$es_v <- as.factor(obj$df$es_v)

  fixed_plot <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$fixed_power, linetype = obj$df$es_v)) +
    p_aes +
    ggtitle("Fixed-Effects Model") +
    labs(linetype = "Effect Size")

  random_plot <- ggplot(rand_dat, aes(x = rand_dat$k_v, y = rand_dat$power, group = rand_dat$power_type, color = rand_dat$power_type)) +
    p_aes +
    ggtitle("Random-Effects Model") +
    scale_color_manual(name = "Heterogenity",
                       labels = c("Large", "Moderate", "Low"),
                       values = c("red","blue","green"))

  p <- cowplot::plot_grid(fixed_plot, random_plot, ncol = 1)

  }

  return (p)

}
