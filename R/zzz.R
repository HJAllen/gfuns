.onLoad <- function(libname, pkgname){

  packageStartupMessage(paste(pkgname, "attached"))

  invisible()

}

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
  suppressPackageStartupMessages((get("library", baseenv()))("gfuns"))
}
