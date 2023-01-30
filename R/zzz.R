# Delay loading arinfopy
# global reference to arinfopy (will be initialized in .onLoad)
ap <- NULL

.onLoad <- function(libname, pkgname) {
    # use super-assignment to update global reference to arinfopy
    # reticulate::use_python("python")
    ap <- reticulate::import("arinfopy", delay_load = TRUE)
    
    utils::globalVariables(c("level", "..level.."))
}
