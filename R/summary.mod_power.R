#' @export

summary.mod_power <- function(object,...) {
  cat("\n Power Analysis for Moderator Analysis: \n\n")

  cat(" Effect Size Metric:               ", object$es_type, "\n")
  cat(" Number of Categorical Groups:     ", object$n_groups, "\n")
  cat(" Groups:                           ", object$group, "\n")
  cat(" Expected Effect Sizes:            ", object$effect_sizes, "\n")
  cat(" Expected Study Size:              ", object$study_size, "\n")
  cat(" Expected Number of Studies:       ", object$k, "\n\n")

  cat(" Esimated Power: Moderator Analysis \n\n")

  cat(" Fixed-Effects Model:              ", object$mod_power$fixed_power_b, "\n")
  cat(" Random-Effects Model (i2 = ",round(object$i2*100,2), "%):   ", object$mod_power$random_power_b, "\n", sep = "")
}
