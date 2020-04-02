#' @export

print.mpower <- function(x,...) {

    cat("\n Estimated Meta-Analytic Power \n\n")

    cat(" Expected Effect Size:             ", x$effect_size, "\n")
    cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
    cat(" Expected Number of Studies;       ", x$k, "\n")
    cat(" Expected between-study sd:        ", x$sd, "\n\n")

    cat(" Estimated Power: Main effect \n\n")

    cat(" Fixed-Effects Model                            ", x$power$fixed_power, "\n")
    cat(" Random-Effects Model (Low Heterogenity):       ", x$power$random_power_s, "\n")
    cat(" Random-Effects Model (Moderate Heterogeneity): ", x$power$random_power_m, "\n")
    cat(" Random-Effects Model (Large Heterogeneity):    ", x$power$random_power_l, "\n\n")

    cat(" Estimated Power: Test of Homogenity \n\n")

    cat(" Fixed-Efects Model                             ", x$homo_power$fixed_power, "\n")
    cat(" Random-Effects Model (Low Heterogeneity):      ", x$homo_power$random_power_s, "\n")
    cat(" Random-Effects Model (Moderate Heterogeneity): ", x$homo_power$random_power_m, "\n")
    cat(" Random-Effects Model (Large Heterogeneity):    ", x$homo_power$random_power_l, "\n")

    #cat(" Fixed-Effects (): ", x$homo_test)

}

