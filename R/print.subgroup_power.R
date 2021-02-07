#' @export

print.subgroup_power <- function(x,...) {
  cat("\n Power Analysis for Subgroup analysis: \n\n")

  cat(" Effect Size Metric:               ", x$es_type, "\n")
  cat(" Number of Subgroups:              ", x$n_groups, "\n")
  cat(" Groups:                           ", x$group, "\n")
  cat(" Expected Effect Sizes:            ", x$effect_sizes, "\n")
  cat(" Expected Study Size:              ", x$study_size, "\n")
  cat(" Expected Number of Studies:       ", x$k, "\n\n")

  cat(" Esimated Power to detect subgroup differences \n\n")

  cat(" Fixed-Effects Model:              ", x$subgroup_power$fixed_power_b, "\n")
  cat(" Random-Effects Model (i2 = ",round(x$i2*100,2), "%):   ", x$subgroup_power$random_power_b, "\n", sep = "")
}
