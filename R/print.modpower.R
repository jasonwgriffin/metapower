#' @export

print.modpower <- function(x,...) {
  cat("\n Estimated Power for Categorical Moderators: ", toupper(x$model),"-effects Model \n\n", sep = "")
  cat(" Number of groups:                             ", x$n_groups, "\n")
  cat(" Expected Effect Sizes:                        ", x$effect_sizes, "\n")
  cat(" Expected Sample Size(per group):              ", x$sample_size, "\n")
  cat(" Expected Number of Studies;                   ", x$k, "\n")
  cat(" Expected heterogenity(t^2):                   ", x$hg, "\n\n")
  cat(" Estimated Power for between-group moderation: ", x$power_b, "\n")
  cat(" Estimated Power for within-group moderation:  ", x$power_w)
}
