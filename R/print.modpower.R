#' @export

print.modpower <- function(x,...) {
  cat("\n Power Analysis for Categorical Moderators: \n\n")

  cat(" Number of groups:                 ", x$n_groups, "\n")
  cat(" Expected Effect Sizes:            ", x$effect_sizes, "\n")
  cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
  cat(" Expected Number of Studies:       ", x$k, "\n\n")

  cat(" Esimated Power \n\n")

  cat(" Fixed-Effects Model (Between-Group):                         ", x$mod_power$fixed_power_b, "\n")
  cat(" Fixed-Effects Model (Within-Group):                          ", x$mod_power$fixed_power_w, "\n")
  cat(" Random-Effects Model (Between-Group, Small Heterogneity):    ", x$mod_power$random_power_b_s, "\n")
  cat(" Random-Effects Model (Between-Group, Moderate Heterogneity): ", x$mod_power$random_power_b_m, "\n")
  cat(" Random-Effects Model (Between-Group, Large Heterogneity):    ", x$mod_power$random_power_b_l, "\n")

}
