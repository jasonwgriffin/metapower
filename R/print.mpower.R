#' @export

print.mpower <- function(obj) {
  cat("\n Estimated Power Analysis for: ", toupper(obj$model),"-effects Model \n\n", sep = "")
  cat(" Expected Effect Size:                    ", obj$effect_size, "\n")
  cat(" Expected Sample Size:                    ", obj$sample_size, "\n")
  cat(" Expected Number of Studies;              ", obj$k, "\n")
  cat(" Expected heterogenity(t^2):              ", obj$hg, "\n\n")
  cat(" Estimated Power:                         ", obj$power, "\n")
  cat(" Estimated Power for Test of Homogeneity: ", obj$homo_test)
}
