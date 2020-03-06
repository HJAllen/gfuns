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

#' Set paths
#'
#' Set paths for easy access
#'
#' This function sets paths based on the workspace and working directory. workspace is the path to the base workspace and the working directory is project specific.
#' @return
#' A list named paths is assigned to the global environment.
#' @param workspace Path to workspace
#' @importFrom magrittr %>%
#' @export
setpaths <- function(workspace = NULL){
  if(!exists("verbose")){
    verbose <- FALSE
  }

  # Set workspace if NULL
  if(is.null(workspace)){
    # Check for OneDrive
    OneDrive <- Sys.getenv("OneDrive")

    # Set root based on OneDrive availability
    workspace <-
      normalizePath(
        file.path(ifelse(OneDrive != "",
                         OneDrive,
                         Sys.getenv("HomePath"))),
        winslash = "/", mustWork = TRUE)
  }

  # Create path list
  paths <-
    tibble::tribble(
      ~Name, ~Path,
      # ws = workspace
      "ws", normalizePath(file.path(workspace, "workspace"),
                          winslash = "/", mustWork = FALSE),
      # lh = Lake-Harsha directory
      "lh", normalizePath(file.path(workspace, "workspace/Lake-Harsha"),
                          winslash = "/", mustWork = FALSE),
      # wd = working directory
      "wd", normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      # wdp = working directory parent
      "wdp", normalizePath(file.path(dirname(getwd())),
                           winslash = "/", mustWork = FALSE),
      # L: drive
      "netmc", normalizePath(file.path("L:/Priv/Cin/ORD/LakeHarshaMC"),
                             winslash = "/", mustWork = FALSE)
    ) %>%
    purrr::pmap(.f = function(Name, Path){
      dirs <-
        list.dirs(normalizePath(
          Path,
          winslash = "/", mustWork = FALSE),
          full.names = FALSE, recursive = FALSE)
      dirs <- dirs[!grepl("^\\.", dirs)]

      tibble::tibble(Name = c(Name, paste(Name, dirs, sep = "_")),
                     Path = c(Path, file.path(Path, dirs))) %>%
        tibble::deframe() #%>%
    }) %>%
    purrr::flatten() %>%
    .[order(names(.))]

   assign("path", paths, envir = .GlobalEnv)
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
