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
#' @param depth integer, depth of directories to recurse
#' @param verbose logical to provide diagnostic feedback
#' @export
setpaths <-
  function(workspace = NULL,
           otherPaths = NULL,
           recurse = NULL,
           depth = 1,
           verbose = F){

    # Testing
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
      recurse <- NULL#list("LHdata")
      depth <- 1
    }

    if(verbose){
      message("gfuns::setpaths: Initial workspace")
      # print(workspace)
      message("gfuns::setpaths: Initial otherPaths")
      # print(otherPaths)
    }

    wsPaths <-
    # Set workspace if NULL
    if(is.null(workspace)){
      list(ws = gfuns::setWorkspace())
    } else workspace

    # if otherPaths given, add to wsPaths
    if(!is.null(otherPaths)){
      wsPaths <- c(wsPaths, otherPaths)
    }

    # Convert to data.frame
    wsPaths <-
      do.call(rbind,
              lapply(names(wsPaths), function(x){
                data.frame(dir = wsPaths[[x]],
                           name = x,
                           level = 0,
                           pName = NA_character_)
              }))

    # Dirs to get recursively
    if(!is.null(recurse) | depth > 0){
      if(is.null(recurse)){
        recurse <- wsPaths
      } else {
        recurse <- wsPaths[wsPaths$name %in% recurse,]
      }
      # for each root path to recurse
      for(root in recurse$name[recurse$level == 0]){
        # for each depth
        for(i in 1:depth){
          # Parent level name
          pNames <- wsPaths$name[grepl(root, wsPaths$name) & wsPaths$level == i - 1]
          for(pName in pNames){
            # for each path to recurse
            pDF <-
              data.frame(dir = list.dirs(wsPaths$dir[wsPaths$name %in% pName], recursive = FALSE))

            # if pDF is N >0
            if(nrow(pDF) > 1){
              pDF$name <- paste(pName, basename(pDF$dir), sep = "_")
              pDF$level <- i
              pDF$pName <- pName
              wsPaths <- rbind(wsPaths, pDF)
            }
          }
        }
      }
    }

    assign("path", stats::setNames(as.list(wsPaths$dir), wsPaths$name), envir = .GlobalEnv)
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
