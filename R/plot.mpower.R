#' @export
plot.mpower <- function(obj){

  # max_k <- obj$df %>%
  #   mutate(power = round(power,3)) %>%
  #   dplyr::filter(power < .9999)
  #
  # obj$df <- obj$df %>%
  #   dplyr::filter(k_v <= max(max_k$k_v))

  if(obj$model == "random"){

    p <- ggplot(obj$df, aes(x = k_v, y = power, linetype = hg_v))
    } else if (obj$model == "fixed"){
    p <- ggplot(obj$df, aes(x = k_v, y = power))
  }

  (p <- p + geom_line(size = 1) +
      scale_x_continuous(limits = c(2,max(obj$df$k_v)), breaks = c(seq(2,max(obj$df$k_v),by = round(max(obj$df$k_v)*.10,0)))) +
      scale_y_continuous(limits =c(0,1), breaks = c(0,.25,.5,.75,1)) +
      geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3)) +
      theme_light()
return (p)
  }


