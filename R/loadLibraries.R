#' Load Libraries
#'
#' Load libraries if not already loaded
#' @param l character vector of libraries to load
#' @export
ll <- function(l = NULL){
  if(!is.null(l)){
    # load Libraries
    for(p in l){
      # print(p)
      if(!p %in% (.packages())){
        suppressPackageStartupMessages(
          library(p, character.only = TRUE)
        )
      }
    }
    # rm("p")
  } else message("l is NULL, no libraries loaded")
}

#' Reload Libraries
#'
#' Reload libraries if already loaded
#' @param l character vector of libraries to load
#' @export
rll <- function(l = NULL){
  if(!is.null(l)){
    # reload Libraries
    for(p in l){
      if(any(grepl(p, search()))){
        do.call("detach", list(name = paste0("package:", p), unload = TRUE))
        message(paste("Package", p, "unloaded"))
      } else{
        message(paste(p, "Is not loaded"))
      }
      if(!p %in% .packages()){
        suppressPackageStartupMessages(
          library(p, character.only = TRUE)
        )
        message(paste("Package", p, "has been loaded"))
      }
    }
  } else message("l is NULL, no libraries loaded")
}
