# This is not a core functionality of exoR. Probably should be removed to scripts.
# Or generalize and move to gfuns
# Add environment argument to be able to specify where loaded.

#' Load Rdata object if exists or excecute function which may generate Rdata object.
#'
#' Initial parsing of data files saved to to Robjects dir
#' Subsequent use will load the specific Rdata file
#' @param RobjFP Full path to Rdata file.
#' @param altFunc Function to execute if .Rdata file does not exist or Force = TRUE.
#' @param Robj Character string to rename object. If NULL, the default, the basename of RobjFP without extension is used.
#' @param env Environment in which to load Robj. Default is .GlobalEnv.
#' @param saveRobj Boolean. If TRUE, the default, and altFunc executed, should the new object be saved to RobjFP directory.
#' @param Force Boolean. If TRUE, Robj is removed and and altFunc executed.
#' @param verbose give feedback
#' @export
loadRdata <- function(RobjFP,
                      altFunc = NULL,
                      Robj = NULL,
                      env = NULL,
                      saveRobj = TRUE,
                      Force = FALSE,
                      verbose = FALSE,
                      ...){

  # Test
  if(FALSE){
    RobjFP <- RobjFPs[1]#file.path(path$ws_Robjects, "tmp.Rdata")
    altFunc <- NULL
    Robj <- NULL
    env <- NULL
    saveRobj <- TRUE
    Force <- !FALSE
    verbose <- !FALSE
    #
    # altFunc <- function() tmp <- "A"

    # gfuns::loadRData(RobjFP = file.path(path$ws_Robjects, "exo_2017.Rdata"),
    #               Force = !FALSE,
    #               verbose = FALSE)
  }

  # Is filePath valid
  if(!dir.exists(dirname(RobjFP))){
    stop("gfuns::loadRdata: RobjectPath directory does not exist.")
  }

  # Set Robjects file name
  RobjF <- basename(RobjFP)

  # Set Robjects file name
  if(is.null(Robj)){
    Robj <- gsub("\\.[A-z]+$", "", RobjF)
  }

  # Set environment
  if(is.null(env)){
    env <- .GlobalEnv
  }

  # If Force, remove Robj from RobjectsPath and .GlobalEnv
  if(Force){
    if(file.exists(RobjFP)){
      if(verbose) message(paste("gfuns::loadRdata: Deleting file:", basename(RobjFP)))
      file.remove(RobjFP)
      if(verbose){
        if(file.exists(RobjFP)){
          message(paste("gfuns::loadRdata: ", basename(RobjFP), "not removed"))
        } else message(paste("gfuns::loadRdata:", basename(RobjFP), "removed"))
      }
    }
    # Remove Robj from env
    if(exists(eval(Robj, envir = env))){
      if(verbose) message(paste("gfuns::loadRdata: Removing", Robj))
      rm(list = eval(Robj), envir = env)
      if(verbose){
        if(exists(eval(Robj))){
          message(paste("gfuns::loadRdata:", Robj, "not removed."))
        } else message(paste("gfuns::loadRdata:", Robj, "removed."))
      }
    }
  }

  # If RobjFP doesn't exist, create it, else load it
  if(!file.exists(RobjFP)){
    # Execute altFunc and assign to env
    if(verbose){
      message(paste("gfuns::loadRdata: Executing altFunc."))
    }

    # If altFunc not given play nice
    if(!exists("altFunc")){
      message("Robj not found and altFunc not provided.")
      return("Robj not found and altFunc not provided.")
    }

    altFuncResult <-
      tryCatch({
        # Return results of altFunc
        do.call(altFunc, args = list(arg = NULL))
      },
      error = function(e){
        emsg <- paste("gfuns::loadRdata: Execution of altFunc failed with error", e)
        attr(emsg, "isError") <- TRUE
        emsg
      })

    # If altFunc is not error
    if(is.null(attr(altFuncResult, "isError"))){
      # Assign to global env
      assign(Robj, altFuncResult, envir = .GlobalEnv)

      # Write files to .Rdata
      if(saveRobj){
        if(verbose) message(paste("gfuns::loadRdata: Saving", Robj, "to", basename(RobjFP)))
        save(list = Robj, file = RobjFP)
      }

    } else {
      message(altFuncResult)
    }

  } else {
    # RobjFP exists and Force is FALSE
    # First remove Robj from env if exists
    if(exists(eval(Robj))){
      if(verbose) message(paste("gfuns::loadRdata: Removing", Robj, "from env."))
      # remove Robj
      rm(list = eval(Robj), envir = env)

      # Verify removal from env
      if(verbose){
        if(exists(eval(Robj))){
          message(paste("gfuns::loadRdata:", Robj, "not removed from env."))
        } else message(paste("gfuns::loadRdata:", Robj, "removed from env."))
      }
    }

    # Load RobjFP to env
    loadResult <-
      load(RobjFP, envir = env)
    if(verbose){
      if(exists(eval(Robj))){
        message(paste("gfuns::loadRdata:", Robj, "added to env."))
      } else message(paste("gfuns::loadRdata:", Robj, "not added to env."))
    }
  }

  # return invisibly whethere alfFuncResult is error
  return(
    invisible(
      if(exists("altFuncResult")){
        if(is.null(attr(altFuncResult, "isError"))){
          # Return Robj name
          Robj
        } else {
          # Return error
          altFuncResult
        }
      } else{
        # Return loadResult
        loadResult
      }
    )
  )
}


#' Load subset or all .RData objects from submitted path
#' @param Path path to Robjects
#' @param objectList list of objects to load
loadRdata2 <-
  function(Path = gfuns::sg("lh_Robjects"), objectList = NULL){
    # If Path doesn't exist, quit
    if(!dir.exists(Path)){
      warning(paste("Path does not exist. Data not loaded."))
      return()
    }
    # if no object list, get all with .RData extension
    if(is.null(objectList)){
      fList <- list.files(Path, pattern = ".RData", full.names = TRUE)
      # print(fList)
    }else{
      fList <- file.path(Path, paste0(objectList, ".RData"))
      # check if fList exist
      fChk <- file.exists((fList))
      if(!all(fChk)){
        warning(paste(fList[!fChk], "file not found and will not be loaded", "\n"))
        fList <- fList[fChk]
      }
    }
    # toNull suppresses printing of object name
    toNull <-
      lapply(fList, load, .GlobalEnv, verbose = F)
  }
