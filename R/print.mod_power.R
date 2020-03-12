#' @export

print.mod_power <- function(obj) {
  cat("\n Estimated Power for Categorical Moderators: ", toupper(obj$model),"-effects Model \n\n", sep = "")
  cat(" Number of groups:                             ", obj$n_groups, "\n")
  cat(" Expected Effect Sizes:                        ", obj$effect_sizes, "\n")
  cat(" Expected Sample Size(per group):              ", obj$sample_size, "\n")
  cat(" Expected Number of Studies;                   ", obj$k, "\n")
  cat(" Expected heterogenity(t^2):                   ", obj$hg, "\n\n")
  cat(" Estimated Power for between-group moderation: ", obj$power_b, "\n")
  cat(" Estimated Power for within-group moderation:  ", obj$power_w)
}
