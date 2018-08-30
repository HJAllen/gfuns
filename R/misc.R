#' Load subset or all .RData objects from submitted path
#' @param Path path to Robjects
#' @param objectList list of objects to load
#' @export
###################################
loadRData <-
  function(Path = lh_Robjects, objectList = NULL){
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

###########################################
#' Plot knit Function
#'
#' This function dynamically produces a knitr chunk with appropriate captions and labels which is then knitted into the main document with cat.  Any chunk option could be added for full control over chunk.
#' @param chunkLabel text for chunk label which is also used for figure file name
#' @param capt text for caption
#' @param plt plot object to be placed
#' @param pos latex option for keeping the plot here
#' @export
plotKknit <- function(chunkLabel, capt, plt, pos='H', ...)
{
  cat(
    knit(
      text =
        knit_expand(text =
                      "<<{{chunkLabel}},eval=TRUE,fig.pos='{{pos}}',fig.cap='{{capt}}'>>=\nplt\n@")
    )
  )
}

#' Custom ggsave function
#'
#' Allows for multiple file formats
#'
#' @param filename filename
#' @param path path where plots should be saved
#' @param plt plot to be printed
#' @param formats list of formats to be saved
#' @param dpi image resolution
#' @export
ggsave_f <-
  function(filename, path = NULL, plt = last_plot(),
           formats = c("pdf", "png", "wmf"), dpi = 300) {
    # print(match.call())
    # if plt.prefix is not NULL then add
    if(!is.null(path)){
      filename <- file.path(path, filename)
    }else{
      filename <- file.path(wd_plots, filename)
    }
    l_ply(
      seq(1:length(formats)),
      .fun = function(x) {
        # 4:3 perspective
        ggplot2::ggsave(
          filename = paste(filename, '_4.3.', formats[x], sep = ''),
          plot = plt, width = 6, height = 6 / (4/3), units = "in",
          dpi = dpi
        )
        # 16:9 perspective
        ggplot2::ggsave(
          filename = paste(filename, '_16.9.', formats[x], sep = ''),
          plot = plt, width = 7.5 * (16/9), height = 7.5, units = "in",
          dpi = dpi
        )
      }
    )
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
  l_ply(sigs,
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
              laply(x$title,
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
  if(!is.POSIXt(t.df$Date) | !is.POSIXt(t.df$Time)) return("t.df must be POSIX")

  # t.df$Time+days(t.df$Date-floor_date(t.df$Time,'days'))
  lubridate::ymd_hms(paste(t.df$Date,
                           lubridate::hour(t.df$Time), ":",
                           lubridate::minute(t.df$Time), ":",
                           lubridate::second(t.df$Time)), tz = tz(t.df$Date))
}

#' Function to modify ggplot facet_grid panel labels
#' @export
plot_labeller <- function(variable,value){
  return(plot_names[value])
}

###############################################

#' Function to parse linear models for adding to plot
#' @param lm lm object
#' @export
lm_eqn <- function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 4),
            r2 = format(summary(m)$r.squared, digits = 3))

  if (coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }
  as.character(as.expression(eq));
}
#' Function to parse linear models for adding to plot
#' @param lm lm object
#' @export
lm_eqn2 <- function(x) {
  m<-lm(as.formula(paste0(names(x)[2],'~',names(x)[1])),data=x)
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 4),
            r2 = format(summary(m)$r.squared, digits = 3))

  if (coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }
  as.character(as.expression(eq));
}

#' Workaround to suppress unwanted readxl output
#' @export
read_excelq <-  function(...) {
  quiet_read <- purrr::quietly(readxl::read_excel)
  out <- quiet_read(...)
  if(length(c(out[["warnings"]], out[["messages"]])) == 0)
    return(out[["result"]])
  else readxl::read_excel(...)
}


#' Time values in Excel spreadsheets are imported as Posix
#'
#' Defaults to the origin date and UTC timezone of microsoft excel which is non-standard. This function extracts the hour, minute, second values and creates a lubridate hms object with specified timezone which can then be combined with _Date to generate _DateTime value Timezone Etc/GMT-6 is Eastern Standard Time
#' @param date_ POSIX date
#' @param time_ POSIX time
#' @param tZ timezone
#' @export
timeExcel_Posix <-
  function(date_, time_, tZ = Tzone){
    year_ <- lubridate::year(date_)
    month_ <- lubridate::month(date_)
    day_ <- lubridate::day(date_)
    hour_ <- lubridate::hour(time_)
    minute_ <- lubridate::minute(time_)
    DT <- paste(
      paste(year_, month_, day_, sep = "-"),
      paste(hour_, minute_, sep = ":"))
    lubridate::ymd_hm(DT, tz = tZ)
  }


