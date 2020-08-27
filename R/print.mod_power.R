#' @export

print.mod_power <- function(x,...) {
  cat("\n Power Analysis for Subgroup analysis: \n\n")

  cat(" Number of Groups:                 ", x$n_groups, "\n")
  cat(" Expected Effect Sizes:            ", x$effect_sizes, "\n")
  cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
  cat(" Expected Number of Studies:       ", x$k, "\n\n")

  cat(" Esimated Power to detect subgroup differences \n\n")

  cat(" Fixed-Effects Model:                 ", x$mod_power$fixed_power_b, "\n")
  #cat(" Fixed-Effects Model (Within-Group):                          ", x$mod_power$fixed_power_w, "\n")
  cat(" Random-Effects Model (i2 = 0%):      ", x$mod_power$random_power_b_0, "\n")
  cat(" Random-Effects Model (i2 = 25%):     ", x$mod_power$random_power_b_s, "\n")
  cat(" Random-Effects Model (i2 = 50%):     ", x$mod_power$random_power_b_m, "\n")
  cat(" Random-Effects Model (i2 = 75%):     ", x$mod_power$random_power_b_l, "\n")
}
