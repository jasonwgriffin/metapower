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
                scale_y_continuous(limits =c(0,1), breaks = c(0,.20,.40,.60,.80,1)),
                ylab("Power"),
                theme_bw(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

      fixed_plot <- ggplot(obj$homogen_power_range %>%
                             dplyr::select(c("k_v",dplyr::starts_with("fixed"))) %>%
                             tidyr::pivot_longer(-"k_v", names_to = "power_type", values_to = "power"), aes(x = .data$k_v, y = .data$power, color = .data$power_type)) +
        geom_line(size = 1) +
        ggtitle("Fixed-Effects Model") + p_aes +
        geom_point(aes(x = obj$k, y = obj$homogen_power$fixed_power_sd1),shape = 21, size = 3, fill = "white",color = "#33CCCC", stroke = 1) +
        geom_point(aes(x = obj$k, y = obj$homogen_power$fixed_power_sd2),shape = 21, size = 3, fill = "white",color = "#6699FF", stroke = 1) +
        geom_point(aes(x = obj$k, y = obj$homogen_power$fixed_power_sd3),shape = 21, size = 3, fill = "white",color = "#9966FF", stroke = 1) +
        geom_point(aes(x = obj$k, y = obj$homogen_power$fixed_power_sd4),shape = 21, size = 3, fill = "white",color = "#9900CC", stroke = 1) +
        geom_point(aes(x = obj$k, y = obj$homogen_power$fixed_power_sd5),shape = 21, size = 3, fill = "white",color = "#993366", stroke = 1) +
        xlab("Number of Studies") +
        scale_color_manual(name = "Standard Deviation \n between Studies",
                           labels = c("SD = 1",
                                      "SD = 2",
                                      "SD = 3",
                                      "SD = 4",
                                      "SD = 5"),
                           values = rev(c("#993366", "#9900CC", "#9966FF", "#6699FF", "#33CCCC")))

      random_plot <- ggplot(obj$homogen_power_range %>% tidyr::pivot_longer("random_power",names_to = "power_type", values_to = "power"), aes(x = .data$k_v, y = .data$power, color = .data$power_type)) +
        geom_line(size = 1)+
        #scale_x_continuous(limits = c(2,max(obj$homo_range$k_v)), breaks = c(seq(2,max(obj$homo_range$k_v),by = round(max(obj$homo_range$k_v)*.10,0)))) +
        p_aes +
        geom_point(aes(x = obj$k, y = obj$homogen_power$random_power),shape = 21, size = 3, fill = "white",color = "#993366", stroke = 1) +
        ggtitle("Random-Effects Model") +
        xlab("Number of Studies") +
        scale_color_manual(name = "Heterogeneity",
                           labels = paste0("i2 = ", round(obj$i2*100,2), "%"),
                           values = rev(c("#993366")))

      p <- cowplot::plot_grid(fixed_plot,random_plot,ncol=1)

  return (p)
}
