#' Launch shiny application of metapower
#'
#' Calling this functioning simply launches the shiny application for metapower,
#' which allows users to easily compute power analysis with a shiny application
#'
#' @return Launches shiny application
#'
#' @examples
#' shiny_metapower()
#'
#'
#' @export

shiny_metapower <- function() {
  appDir <- system.file("shiny_metapower", package = "metapower")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `metapower`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
