#' Launch the ShinyApp
setwd('~/Dropbox/Projects/GitHub/CytoDA/')
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
enableBookmarking(store = "url")
CytoDA::run_app()

