# .onAttach <- function(libname, pkgname) {
#     packageStartupMessage("Welcome to simulariatools")
# }

# Delay loading arinfopy
# global reference to arinfopy (will be initialized in .onLoad)
ap <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global reference to scipy
    reticulate::use_python("python3")
    ap <- reticulate::import("arinfopy", delay_load = TRUE)
}