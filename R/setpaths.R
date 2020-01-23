#' Shortcut to Sys.getenv
#'
#' Sys.getenv is too much typing
#'
#' This function is included for backward compatibility. Paths are now in a list named path and are accessed via path$pathName or path[[`pathName`]]. Previously, paths were set in the system enironment. It is still capable of returning a system environment variable as it looks for x in the system environment before path.
#' @param x Environment variable string
#' @export
sg <- function(x){
  if(Sys.getenv(x) != ""){
    Sys.getenv(x)
  } else if(!is.null(path[[x]])){
    path[[x]]
  } else stop(paste("path", x, "is not set."))
}

#' Get workspace
#'
#' Helper for setpaths to establish workspace programatically
#'
#' This function sets the workspace based on OS type and OneDrive availability.
#' @return
#' character value representing workspace directory.
#' @importFrom magrittr %>%
#' @export
setWorkspace <- function(){

  # If Windows
  if(grepl("(?i)windows", .Platform$OS.type)){
    # Check for OneDrive
    OneDrive <- Sys.getenv("OneDrive")

    # Set root based on OneDrive availability
    normalizePath(
      file.path(ifelse(OneDrive != "",
                       OneDrive,
                       Sys.getenv("HomePath")),
                "workspace"),
      winslash = "/", mustWork = TRUE)
  } else if(grepl("(?i)nix", .Platform$OS.type)){
    # Unix type
    normalizePath(
      file.path(Sys.getenv("HOME"),
                "workspace"),
      winslash = "/", mustWork = TRUE)
  }
}

#' Set paths
#'
#' Set paths for easy access
#'
#' This function sets paths based on the workspace and working directory. workspace is the path to the base workspace and the working directory is project specific. By default, workspace toplevel directories are returned. If recurse is given, return listed directories recursively.
#' @return
#' A list named paths is assigned to the global environment.
#' @param workspace Path to workspace
#' @param otherPaths named list of paths outside of workspace to include
#' @param recurse list of directories to get recursively
#' @export
setpaths <-
  function(workspace = NULL,
           otherPaths = NULL,
           recurse = NA){

    # Testiing
    if(!T){
      workspace <- NULL
      otherPaths <-
        list(
          wd = getwd(),
          LMC = normalizePath(file.path("L:/Priv/Cin/ORD/LakeHarshaMC"),
                              winslash = "/", mustWork = FALSE),
          CHL = normalizePath(file.path("L:/Priv/Cin/NRMRL/Chlorophyll"),
                              winslash = "/", mustWork = FALSE),
          L35 = normalizePath(file.path("L:/Lab/Lablan/MSE/Lamba35"),
                              winslash = "/", mustWork = FALSE))
      recurse <- list("LHdata")
    }

    # Set workspace if NULL
    if(is.null(workspace)){
      workspace <- list(ws = gfuns::setWorkspace())
    }

    # if otherPaths given, add to workspace
    if(!is.null(otherPaths)){
      workspace <- c(workspace, otherPaths)
    }

    # Create path list starting with dirs in workspace
    wsPaths <-
      unlist(
        lapply(names(workspace), function(x){
          # print(workspace[[x]])
          c(stats::setNames(workspace[[x]], x),
            as.list(
              list.dirs(workspace[[x]], recursive = FALSE)) %>%
              stats::setNames(gsub(" ", "_", paste0(x, "_", basename(unlist(.))))))
        }),
        recursive = FALSE)

    # Dirs to get recursively
    if(!is.na(recurse)){
      for(x in recurse){
        for(i in which(grepl(x, wsPaths))){
          Path <- list.dirs(as.character(wsPaths[i]))[-1]
          if(length(Path) > 0){
            pNames <- regmatches(Path, gregexpr(paste0("(?i)", x, ".+$"), Path))
            pNames <- gsub("[\\s\\/]", "_", pNames, perl = TRUE)
            names(Path) <- pNames
            wsPaths <- c(wsPaths, Path)
          }
        }
      }
    }

    assign("path", wsPaths, envir = .GlobalEnv)
    if(verbose) message("path set")
}

#' #' Set paths
#' #'
#' #' Set paths relative to working directory
#' #'
#' #' @param OneDrive logical flag indicating if OneDrive is used
#' #' @param SubD character vector of subdirectories to use to build paths
#' # #' @export
#' setpaths <- function(OneDrive = TRUE,
#'                      SubD = c('defaults', 'data', 'Robjects', 'figures', 'plots')){
#'
#'   OD <- Sys.getenv("OneDrive")
#'   HP <- Sys.getenv("HomePath")
#'   # Create root path list
#'   roots <-
#'     list(# ws = workspace
#'       ws = list(
#'         Name = "ws",
#'         Path = if(OneDrive){
#'           normalizePath(file.path(OD,"workspace"))
#'         }else{normalizePath(file.path(HP,"workspace"))},
#'         SubD = SubD[1]),
#'       # lh = Lake-Harsha directory
#'       lh = list(
#'         Name = "lh",
#'         Path = if(OneDrive){
#'           normalizePath(file.path(OD,"workspace/Lake-Harsha"))
#'         }else{normalizePath(file.path(HP,"workspace/Lake-Harsha"))},
#'         SubD = SubD),
#'       # wd = working directory
#'       wd = list(
#'         Name = "wd",
#'         Path = normalizePath(getwd()),
#'         SubD = SubD),
#'       # wdp = working directory parent
#'       wdp = list(
#'         Name = "wdp",
#'         Path = normalizePath(file.path(dirname(getwd()))),
#'         SubD = SubD),
#'       # L: drive
#'       netmc = list(
#'         Name = "netmc",
#'         Path = normalizePath(file.path("L:/Priv/Cin/ORD/LakeHarshaMC")),
#'         SubD = NULL)
#'     )
#'
#'   # Create path handles
#'   purrr::walk(roots,#[grepl("w|lh", names(roots))],
#'               function(x){
#'                 # Set root path handles
#'                 if(dir.exists(x$Path)){
#'                   tryCatch({
#'                     assign(x$Name, x$Path, envir = .GlobalEnv)
#'                     message(paste(x$Name, x$Path, sep = " = "))
#'                   },
#'                   warning = function(war){
#'                     message(paste(x$Name, "Not Set"))
#'                   },
#'                   error = function(err){
#'                     message(paste(x$Name, "Not Set"))
#'                   })
#'                 }
#'                 # Set root subPath handles
#'                 purrr::walk(x$SubD,
#'                             function(y){
#'                               Name_ <- paste(x$Name, y, sep="_")
#'                               Path_ <- normalizePath(file.path(x$Path, y), mustWork = FALSE)
#'                               if(dir.exists(Path_)){
#'                                 tryCatch({
#'                                   assign(Name_,
#'                                          Path_,
#'                                          envir = .GlobalEnv)
#'                                   message(paste(Name_, Path_, sep = " = "))
#'                                 },
#'                                 warning = function(war){
#'                                   message(paste(Name_, "Not Set"))
#'                                 },
#'                                 error = function(err){
#'                                   message(Name_)
#'                                 }
#'                                 )
#'                               }
#'                             }
#'                 )
#'               }
#'   )
#' }
