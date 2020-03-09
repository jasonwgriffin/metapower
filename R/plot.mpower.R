#' @export
plot.mpower <- function(obj){

  if(obj$model == "random"){
    p <- ggplot(obj$df, aes(x = k_v, y = power, color = obj$df$hg_v))
  } else if (obj$model == "fixed"){
    p <- ggplot(obj$df, aes(x = k_v, y = power))
  }

  (p <- p + geom_line(size = 1) + scale_x_continuous(breaks = c(5,10,15,20,25,30)) + theme_light())

}
