#' @export

print.mpower <- function(x,...) {

  if (x$model == "fixed"){

    cat("\n Estimated Power Analysis for: ", toupper(x$model),"-effects Model \n\n", sep = "")
    cat(" Expected Effect Size:                    ", x$effect_size, "\n")
    cat(" Expected Sample Size:                    ", x$sample_size, "\n")
    cat(" Expected Number of Studies;              ", x$k, "\n")
    cat(" Expected between-study sd:               ", x$sd, "\n\n")
    cat(" Estimated Power:                         ", x$power, "\n")
    cat(" Estimated Power for Test of Homogeneity: ", x$homo_test)

  } else if (x$model == "random"){

    cat("\n Estimated Power Analysis for: ", toupper(x$model),"-effects Model \n\n", sep = "")
    cat(" Expected Effect Size:                    ", x$effect_size, "\n")
    cat(" Expected Sample Size:                    ", x$sample_size, "\n")
    cat(" Expected Number of Studies;              ", x$k, "\n")
    cat(" Expected heterogenity (tau^2):           ", x$hg, "\n\n")
    #cat(" Expected between-study sd:               ", x$sd, "\n\n")
    cat(" Estimated Power:                         ", x$power, "\n")
    cat(" Estimated Power for Test of Homogeneity: ", x$homo_test)

  }
}
