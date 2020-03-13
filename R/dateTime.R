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

#' Convert to UTC
#'
#' Given POSIXct convert time to UTC
#' @param DT vector of DateTimes in POSIXct
#' @param tz Timezone to convert to, defaults to "UTC"
#' @export
toUTC <- function(DT, tz = "UTC"){

  # Dev
  if(FALSE){
    DT <- seq.POSIXt(as.POSIXct("2010-01-01 12:00:00", tz = "EST5EDT"), by = 60*60*3, length.out = 5)
    tz <- "UTC"
    attributes(DT)
  }

  as.numeric(DT)
  lubridate::with_tz(DT, tzone = tz)
  attributes(as.POSIXct(DT, tz = tz))
  as.numeric(as.POSIXct(DT, tz = tz))

}
