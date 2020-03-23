#' Plot power curve
#'
#' @export
#'
#' @param obj This should be an mpower object
#'
#' @return Power curve plot for the user specified input parameters
#'

power_plot <- function(obj){

if(obj$es_type == "d"){
  if (obj$model == "fixed"){

    obj$df$EffectSize <- as.factor(obj$df$EffectSize)
    p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$EffectSize))

  } else if (obj$model == "random"){

    p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$Heterogenity))

  }

  p <- p + geom_line(size = 1) +
      scale_x_continuous(limits = c(2,max(obj$df$k_v)), breaks = c(seq(2,max(obj$df$k_v),by = round(max(obj$df$k_v)*.10,0)))) +
      scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)) +
      geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3) +
    xlab("Number of Studies") +
    ylab("Power") +
    ggtitle("Estimated Meta-analytic power for main effects") +
    labs(linetype = "Effect Size") +
    theme_classic() +
      theme(legend.position = c(1,0),
            legend.justification = c(1,0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

  return (p)

  }else if(obj$es_type == "Correlation"){
    if (obj$model == "fixed"){

      obj$df$EffectSize <- as.factor(obj$df$EffectSize)
      p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$EffectSize)) +
        labs(linetype = "Effect Size(Correlation)")

    } else if (obj$model == "random"){

      p <- ggplot(obj$df, aes(x = obj$df$k_v, y = obj$df$power, linetype = obj$df$Heterogenity)) +
        labs(linetype = "Heterogenity)")

    }

    p <- p + geom_line(size = 1) +
      scale_x_continuous(limits = c(2,max(obj$df$k_v)), breaks = c(seq(2,max(obj$df$k_v),by = round(max(obj$df$k_v)*.10,0)))) +
      scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)) +
      geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3) +
      xlab("Number of Studies") +
      ylab("Power") +
      ggtitle("Estimated Meta-analytic power for r-fisher's z transformation") +
      theme_classic() +
      theme(legend.position = c(1,0),
            legend.justification = c(1,0),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

    return (p)






  }else if(obj$es_type =="OR"){

  }
}


