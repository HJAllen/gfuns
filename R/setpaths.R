#' Shortcut to Sys.getenv
#'
#' Sys.getenv is too much typing
#'
#' Returns Sys.getenv() value
#' @param x Environment variable string
#' @export
sg <- function(x){
  if(Sys.getenv(x) == ""){
    stop(paste("Environment var", x, "is not set."))
  }else Sys.getenv(x)
}


#' Set paths
#'
#' Set paths relative to working directory
#'
#' @param SubD character vector of subdirectories to use to build paths
#' @importFrom magrittr %>%
#' @export
setpaths <- function(SubD = c('defaults', 'data', 'Robjects', 'figures', 'plots')){
  # Check for OneDrive
  OneDrive <- Sys.getenv("OneDrive") != ""

  # Create path list
  paths <-
    list(# ws = workspace
      ws = list(
        Name = "ws",
        Path = if(Sys.getenv("OneDrive") != ""){
          normalizePath(file.path(Sys.getenv("OneDrive"),"workspace"),
                        winslash = "/", mustWork = FALSE)
        }else{normalizePath(file.path(Sys.getenv("HomePath"),"workspace"),
                            winslash = "/", mustWork = FALSE)},
        SubD = SubD),
      # lh = Lake-Harsha directory
      lh = list(
        Name = "lh",
        Path = if(Sys.getenv("OneDrive") != ""){
          normalizePath(
            file.path(
              Sys.getenv("OneDrive"),
              "workspace/Lake-Harsha"),
            winslash = "/", mustWork = FALSE)
        }else{
          normalizePath(
            file.path(
              Sys.getenv("HomePath"),
              "workspace/Lake-Harsha"),
            winslash = "/", mustWork = FALSE)},
        SubD = SubD),#dir(sg("lh_data"))),
      # lh = Lake-Harsha data directory
      lhd = list(
        Name = "lhd",
        Path = if(Sys.getenv("OneDrive") != ""){
          normalizePath(
            file.path(
              Sys.getenv("OneDrive"),
              "workspace/Lake-Harsha/data"),
            winslash = "/", mustWork = FALSE)
        }else{
          normalizePath(
            file.path(
              Sys.getenv("HomePath"),
              "workspace/Lake-Harsha/data"),
            winslash = "/", mustWork = FALSE)},
        SubD = if(Sys.getenv("OneDrive") != ""){
          list.dirs(
            normalizePath(
              file.path(
                Sys.getenv("OneDrive"),
                "workspace/Lake-Harsha/data"),
              winslash = "/", mustWork = FALSE),
            full.names = FALSE, recursive = FALSE)
        }else{
          list.dirs(normalizePath(
            file.path(
              Sys.getenv("HomePath"),
              "workspace/Lake-Harsha/data"),
            winslash = "/", mustWork = FALSE),
            full.names = FALSE, recursive = FALSE)
        }),
      # wd = working directory
      wd = list(
        Name = "wd",
        Path = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
        SubD = list.dirs(normalizePath(
          getwd(),
          winslash = "/", mustWork = FALSE),
          full.names = FALSE, recursive = FALSE)),
      # wdp = working directory parent
      wdp = list(
        Name = "wdp",
        Path = normalizePath(file.path(dirname(getwd())),
                             winslash = "/", mustWork = FALSE),
        SubD = SubD),
      # L: drive
      netmc = list(
        Name = "netmc",
        Path = normalizePath(file.path("L:/Priv/Cin/ORD/LakeHarshaMC"),
                             winslash = "/", mustWork = FALSE),
        SubD = NULL)
    ) %>%
    purrr::map(function(x){
      {if(!is.null(x$SubD)){
        x$SubD <- x$SubD[!grepl("\\.R", x$SubD)]
        tibble::tibble(name = c(x$Name, paste(x$Name, basename(x$SubD), sep = "_")),
                       path = c(x$Path, normalizePath(file.path(x$Path, basename(x$SubD)),
                                                      winslash = "/", mustWork = FALSE)))
      }else{
        tibble::tibble(name = x$Name,
                       path = x$Path)
      }} %>%
        dplyr::filter(dir.exists(path)) %>%
        tibble::deframe()
    }) %>%
    purrr::flatten()

  do.call(Sys.setenv, paths)

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
