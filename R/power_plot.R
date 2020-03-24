#' Plot power curve
#'
#' @export
#'
#' @param obj This should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters
#'

power_plot <- function(obj){

  if (obj$model == "fixed"){

    obj$df$EffectSize <- as.factor(obj$df$EffectSize)
    p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$EffectSize))

    if(obj$es_type == "d"){
      p <- p + labs(linetype = "Effect Size (d)") +
        ggtitle("Estimated Meta-analytic Power for Cohen's d")
      }else if (obj$es_type == "Correlation"){
      p <- p + labs(linetype = "Effect Size(corr)") +
        ggtitle("Estimated Meta-analytic Power for Correlation")
      }else if (obj$es_type =="OR"){
      p <- p +labs(linetype = "Effect Size(log Odds)") +
        ggtitle("Estimated Meta-analytic Power(log Odds)")
      }

  }else if (obj$model == "random"){

    p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$Heterogenity)) +
      labs(linetype = "Heterogenity")
    if(obj$es_type == "d"){
      p <- p + ggtitle("Estimated Meta-analytic Power for Cohen's d")
    }else if (obj$es_type == "Correlation"){
      p <- p + ggtitle("Estimated Meta-analytic Power for Correlation")
    }else if (obj$es_type == "OR"){
      p <- p + ggtitle("Estimated Meta-analytic Power(log Odds)")
    }

  }

  p <- p + geom_line(size = 1) +
      scale_x_continuous(limits = c(2,max(obj$df$k_v)), breaks = c(seq(2,max(obj$df$k_v),by = round(max(obj$df$k_v)*.10,0)))) +
      scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)) +
      geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3) +
    xlab("Number of Studies") +
    ylab("Power") +
    theme_classic() +
      theme(legend.position = c(1,0),
            legend.justification = c(1,0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

  return (p)

}
