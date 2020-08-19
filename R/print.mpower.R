#' @export

print.mpower <- function(x,...) {

    cat("\n Power Analysis for Meta-analysis \n\n")

    cat(" Expected Effect Size:             ", x$effect_size, "\n")
    cat(" Expected Sample Size (per group): ", x$sample_size, "\n")
    cat(" Expected Number of Studies:       ", x$k, "\n")
    cat(" Expected Between-study SD:        ", x$sd, "\n\n")

    cat(" Estimated Power: Mean Effect Size \n\n")

    cat(" Fixed-Effects Model                    ", x$power$fixed_power, "\n")
    # cat(" Random-Effects Model (Small Heterogeneity):     ", x$power$random_power_s, "\n")
    # cat(" Random-Effects Model (Moderate Heterogeneity): ", x$power$random_power_m, "\n")
    # cat(" Random-Effects Model (Large Heterogeneity):    ", x$power$random_power_l, "\n\n")
    cat(" Random-Effects Model (I2 =   0% ):     ", x$jackson_power$random_power_0, "\n")
    cat(" Random-Effects Model (I2 =  25% ):     ", x$jackson_power$random_power_25, "\n")
    cat(" Random-Effects Model (I2 =  50% ):     ", x$jackson_power$random_power_50, "\n")
    cat(" Random-Effects Model (I2 =  75% ):     ", x$jackson_power$random_power_75, "\n")
    cat(" Random-Effects Model (I2 = 100% ):     ", x$jackson_power$random_power_100, "\n\n")

    cat(" Estimated Power: Test of Homogeneity \n\n")

    cat(" Fixed-Efects Model                    ", x$homo_power$fixed_power, "\n")
    cat(" Random-Effects Model (I2 =   0% ):    ", x$homo_power$random_power_0, "\n")
    cat(" Random-Effects Model (I2 =  25% ):    ", x$homo_power$random_power_25, "\n")
    cat(" Random-Effects Model (I2 =  50% ):    ", x$homo_power$random_power_50, "\n")
    cat(" Random-Effects Model (I2 =  75% ):    ", x$homo_power$random_power_75, "\n")
    cat(" Random-Effects Model (I2 = 100% ):    ", x$homo_power$random_power_100, "\n")

}
