#' Plot power curve
#'
#' @export
#'
#' @param obj This should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters

homogen_power_plot <- function(obj){

  if (obj$model == "fixed"){
    obj$homo_range <- obj$homo_range %>% dplyr::filter(obj$homo_range$k_v == obj$k)

    #obj$homo_range$SD <- as.factor(obj$homo_range$SD)
    p <- ggplot(obj$homo_range, aes(x = obj$homo_range$SD, y = obj$homo_range$power,group = obj$homo_range$k_v)) + geom_line(size = 1) +
      geom_point(aes(x = obj$sd, y = obj$homo_test), shape = 21, color = "black", fill = "red", size = 3) +
      xlab("Number of Studies") +
      ylab("Power") +
      theme_classic()

  } else if (obj$model == "random"){

    p <- ggplot(obj$homo_range, aes(x = obj$homo_range$k_v, y = obj$homo_range$power, linetype = obj$homo_range$Heterogenity)) +
      geom_line(size = 1) +
      scale_x_continuous(limits = c(2,max(obj$homo_range$k_v)), breaks = c(seq(2,max(obj$homo_range$k_v),by = round(max(obj$homo_range$k_v)*.10,0)))) +
      scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)) +
      geom_point(aes(x = obj$k, y = obj$homo_test), shape = 21, color = "black", fill = "red", size = 3) +
      xlab("Number of Studies") +
      ylab("Power") +
      ggtitle("Power for Test of Homogenity") +
      labs(linetype = "Heterogeneity") +
      theme_classic() +
      theme(legend.position = c(1,0),
            legend.justification = c(1,0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
    }

  return (p)
}


