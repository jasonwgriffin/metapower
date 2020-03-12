#' @export
plot.mpower <- function(obj){

  if(obj$model == "random"){

    p <- ggplot(obj$df, aes(x = k_v, y = power, linetype = obj$df$hg_v)) +
      geom_line(size = 1) + scale_x_continuous(limits = c(0,3*k),breaks = seq(0,3*k, by = 5)) + theme_light()

    } else if (obj$model == "fixed"){
    p <- ggplot(obj$df, aes(x = k_v, y = power)) +
      geom_line(size = 1) + scale_x_continuous(limits = c(0,3*k), breaks = seq(0,3*k, by = 5)) + theme_light()
  }
  (p <- p + geom_point(aes(x = obj$k, y = obj$power), shape = 21, color = "black", fill = "red", size = 3))
}
