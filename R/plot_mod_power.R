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
                scale_y_continuous(limits =c(0,1), breaks = c(0,.20,.40,.60,.80,1)),
                xlab("Number of Studies"),
                ylab("Power"),
                theme_bw(),
                theme(legend.position = c(1,0),
                      legend.justification = c(1,0),
                      legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')))

  return(ggplot(obj$mod_power_range %>% tidyr::pivot_longer(c("fixed_power_b", "random_power_b"), names_to = "power_type", values_to = "power"),
                aes(x = .data$k_v, y = .data$power, color = .data$power_type)) + p_aes +
           geom_point(aes(x = obj$k, y = obj$mod_power$fixed_power_b),shape = 21, size = 3, fill = "white", color = "#993366", stroke = 1) +
           geom_point(aes(x = obj$k, y = obj$mod_power$random_power_b),shape = 21, size = 3, fill = "white",color = "#33CCCC", stroke = 1) +
           ggtitle("Power Analysis for Moderator Analysis") +
           scale_color_manual(name = paste0("Moderator Model (",obj$n_groups, " groups)"),
                              labels = c("Fixed Effects", paste0("Random Effects (I2 = ",round(obj$i2*100,2), "%)")),
                              values = c("#993366", "#33CCCC")))
}
