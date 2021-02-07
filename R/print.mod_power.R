#' @export

print.mod_power <- function(x,...) {
  cat("\n Power Analysis for Moderator Analysis: \n\n")

  cat(" Effect Size Metric:               ", x$es_type, "\n")
  cat(" Number of Categorical Groups:     ", x$n_groups, "\n")
  cat(" Groups:                           ", x$group, "\n")
  cat(" Expected Effect Sizes:            ", x$effect_sizes, "\n")
  cat(" Expected Study Size:              ", x$study_size, "\n")
  cat(" Expected Number of Studies:       ", x$k, "\n\n")

  cat(" Esimated Power: Moderator Analysis \n\n")

  cat(" Fixed-Effects Model:              ", x$mod_power$fixed_power_b, "\n")
  cat(" Random-Effects Model (i2 = ",round(x$i2*100,2), "%):   ", x$mod_power$random_power_b, "\n", sep = "")

}
