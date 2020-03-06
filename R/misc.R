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

#' Load subset or all .RData objects from submitted path
#' @param Path path to Robjects
#' @param objectList list of objects to load
loadRData <-
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


#' Signature page table function for latex signature page
#'
#' supply name and titles and output dataframe appropriate for pander::pandoc.table. Used in SOPs and QAPP.  sigs is list of sigs and title
#' @param sigs list of names and affiliation to be included in signatures
#' @param online should name be on line
#' @param vspace latex space between signature entries
#' @examples
#' sigs <- list(list(name = 'Joel Allen',
#'                   title = 'Principle Investigator',
#'                   online = 'test'),
#'              list(name = 'Michael Elovitz',
#'                   title = c('Branch Chief',
#'                             'Water Quality Management Branch')))
#' @export
sigFunc<-function(sigs, online = NULL, vspace = '0.75in'){
  # set \dateline length
  cat('\\ifcsdef{dateline}{}{\\newlength{\\dateline}}\n')
  cat('\\begin{minipage}{\\textwidth}\n')
  plyr::l_ply(sigs,
        .fun = function(x){
          lvar <- gsub('[^a-zA-Z0-9.]', '', x$name)
          cat(paste0('\\setlength\\dateline{0.7\\textwidth}',
                     '\\newlength{\\', lvar,
                     '}\\settowidth{\\', lvar,
                     '}{',x$name,'}'),
              if(!is.null(vspace)){
                paste0('\\vspace{', vspace, '}')
              },
              if(!is.null(x$online)){
                paste0(x$online, '\\newline')
              },
              '\\rule{0.5\\textwidth}{0.5pt}\\hspace{0.2\\textwidth}\\rule{0.3\\textwidth}{0.5pt}',
              paste0('\\advance\\dateline by -\\', lvar),
              paste0(x$name, '\\hspace{\\dateline}Date\\newline'),
              plyr::laply(x$title,
                      .fun = function(y){
                        paste0(y, '\\newline')
                      }),
              '  \n',
              sep = '\n')
        })
  cat('\\end{minipage}')
}

#' Combine factors with different levels
#'
#' while retaining original levels of originals. submit factors as dataframe or list. ordered.levels not implemented but maybe pass the desired order of output levels
#' @export
############################################
cFactor <- function(factors,ordered.levels=NULL){
  tmp <- lapply(factors,as.character)
  tmp <- factor(unlist(tmp))
  names(tmp) <- NULL
  tmp
}

#' Function to adjust Time values as read by read_excel
#'
#' input is a dataframe with two columns labeled Date and Time. Both need to be in the same timezone.
#' @section Value:
#' output is a DateTime vector
#' @param date.v POSIX date vector
#' @param time.v POSIC time vector
#' @export
time2DT <- function(date.v, time.v){
  #check is.null time
  time.v[is.na(time.v)] <- strptime('12:00:00', "%H:%M:%S", tz = 'Etc/GMT-6')


  t.df <- data.frame(Date = date.v, Time = time.v)
  #check format of t.df
  if(!all(colnames(t.df) %in% c('Date','Time'))) return('t.df columns mislabeled')
  if(!lubridate::is.POSIXt(t.df$Date) |
     !lubridate::is.POSIXt(t.df$Time)) return("t.df must be POSIX")

  # t.df$Time+days(t.df$Date-floor_date(t.df$Time,'days'))
  lubridate::ymd_hms(paste(t.df$Date,
                           lubridate::hour(t.df$Time), ":",
                           lubridate::minute(t.df$Time), ":",
                           lubridate::second(t.df$Time)), tz = lubridate::tz(t.df$Date))
}

#' #' Function to modify ggplot facet_grid panel labels
#' #' @export
#' plot_labeller <- function(variable,value){
#'   return(plot_names[value])
#' }


#' Separate multiple columns
#'
#' Takes a data frame with multiple columns to split and returns resulting frame
#' @param .data data frame with columns to split
#' @param .cols columns to split
#' @param .suffix suffix to add to each column name for expansion
#' @param .sep separator string in .cols
#' @export
separate_multi <- function(.data, .cols, .suffix, .sep){
  for(x in .cols){
    .data %<>%
      tidyr::separate(x,
                      c(x, paste(x, .suffix, sep = "_")),
                      sep = .sep)
  }
  .data
}

#' Time values in Excel spreadsheets are imported as Posix
#'
#' Defaults to the origin date and UTC timezone of microsoft excel which is non-standard 1899-12-31. This function extracts the hour, minute, second values and creates a lubridate hms object with specified timezone which can then be combined with _Date to generate _DateTime value Timezone Etc/GMT-6 is Eastern Standard Time
#' @param date_ POSIX date
#' @param time_ POSIX time
#' @param origin date/time origin
#' @param TZin original timezone
#' @param TZout output timezone
#' @export
timeExcel_Posix <-
  function(date_, time_, TZin = "UTC", TZout = "Etc/GMT+5", origin = as.POSIXct("1899-12-31", tz = "UTC")){
    # Get seconds since excel origin
    tSecs <- as.numeric(time_) - as.numeric(origin)
    # Add to Date
    DT <- as.numeric(date_) + tSecs
    as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = TZin), tz = TZout) - Olson$offset[Olson$OlsonNames == TZout]

    # year_ <- lubridate::year(date_)
    # month_ <- lubridate::month(date_)
    # day_ <- lubridate::day(date_)
    # hour_ <- lubridate::hour(time_)
    # minute_ <- lubridate::minute(time_)
    # second_ <- lubridate::second(time_)
    # DT <- paste(
    #   paste(year_,
    #         sprintf("%02d", month_),
    #         sprintf("%02d", day_), sep = "-"),
    #   paste(sprintf("%02d", hour_),
    #         sprintf("%02d", minute_),
    #         sprintf("%02d", second_), sep = ":"))
    # lubridate::ymd_hms(DT, tz = tZ)
  }
