#' @export
#'
#'
#'

shiny_metapower <- function() {
  appDir <- system.file("shiny_metapower", package = "metapower")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `metapower`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
