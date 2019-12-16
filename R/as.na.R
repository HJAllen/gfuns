#' Function to accept a vector and return vector of NAs while retaining class and factor attributes
#'
#' For use when wishing to maintain attributes of vector while changing values to NA
#' @param vec vector of values with class attributes
#' @examples
#' # With data.table
#' library(data.table)
#'
#' DT <- data.table::data.table(
#' int = seq(1, 10),
#' real = seq(1, 10) + 0.1,
#' char = letters[1:10],
#' logi = rep(c(TRUE, FALSE), 5),
#' Date = seq.Date(as.Date("2019-09-03"), by = 1, length.out = 10),
#' posix = seq.POSIXt(as.POSIXct("2019-09-03 12:00:00"), by = 360, length.out = 10),
#' factr = as.factor(LETTERS[1:10]))
#'
#' str(DT)
#'
#' DT_na <-
#'   copy(DT)[, lapply(.SD, as.na)]
#'
#' str(DT_na)
#'
#' # With data.frame
#' DF <- data.frame(
#'   int = seq(1, 10),
#'   real = seq(1, 10) + 0.1,
#'   char = letters[1:10],
#'   logi = rep(c(TRUE, FALSE), 5),
#'   Date = seq.Date(as.Date("2019-09-03"), by = 1, length.out = 10),
#'   posix = seq.POSIXt(as.POSIXct("2019-09-03 12:00:00"), by = 360, length.out = 10),
#'   stringsAsFactors = FALSE
#' )
#'
#' DF$factr <- as.factor(LETTERS[1:10])
#' str(DF)
#' DF_na <- DF
#' for(i in colnames(DF)){
#'   DF_na[,i] <- as.na(DF[,i])
#' }
#'
#' str(DF_na)
#'
#' # Other wasy to do it
#' # From https://stackoverflow.com/questions/13937345/as-na-function/57779377#57779377
#' flodel <- function(x) {x[] <- NA; x}
#' joshU <- function(x) {is.na(x) <- seq_along(x); x}
#' joshO <- function(x) rep(c(x[0], NA), length(x))
#'
#' # To benchmark
#' library(rbenchmark)
#'
#' # Some vectors to  test them on
#' int  <- 1:1e6
#' char <- rep(letters[1:10], 1e5)
#' bool <- rep(c(TRUE, FALSE), 5e5)
#'
#' benchmark(replications=100, order="relative",
#'           flodel_bool = flodel(bool),
#'           flodel_int  = flodel(int),
#'           flodel_char = flodel(char),
#'           joshU_bool = joshU(bool),
#'           joshU_int  = joshU(int),
#'           joshU_char = joshU(char),
#'           joshO_bool = joshO(bool),
#'           joshO_int  = joshO(int),
#'           joshO_char = joshO(char),
#'           as.na_bool = as.na(bool),
#'           as.na_int = as.na(int),
#'           as.na_char = as.na(char))[1:6]
#'
#' benchmark(replications=100, order="relative",
#'           flodel_DT = DT[, lapply(.SD, flodel)],
#'           joshU_DT = DT[, lapply(.SD, joshU)],
#'           joshO_DT = DT[, lapply(.SD, joshO)],
#'           as.na_DT = DT[, lapply(.SD, as.na)])
#'
#' benchmark(replications=100, order="relative",
#'           flodel_DF = {
#'             DF_na <- DF
#'             for(i in colnames(DF)){
#'               DF_na[,i] <- flodel(DF[,i])
#'             }
#'             DF_na
#'           },
#'           joshU_DF = {
#'             DF_na <- DF
#'             for(i in colnames(DF)){
#'               DF_na[,i] <- joshU(DF[,i])
#'             }
#'             DF_na
#'           },
#'           joshO_DF = {
#'             DF_na <- DF
#'             for(i in colnames(DF)){
#'               DF_na[,i] <- joshO(DF[,i])
#'             }
#'             DF_na
#'           },
#'           as.na_DF = {
#'             DF_na <- DF
#'             for(i in colnames(DF)){
#'               DF_na[,i] <- as.na(DF[,i])
#'             }
#'             DF_na
#'           })
#'
#' @export


as.na <- function(vec){
  # Check if vector
  if(!is.atomic(vec)){
    message("vec is not atomic")
    return(NULL)
  }

  # Special case for factors - any others that need to be handled?
  if(is.factor(vec)){
    # handle to retain levels and return
    factor(rep(NA, length(vec)), levels = levels(vec))
  } else{
    # Replicate with NA
    x <- rep(NA, length(vec))
    # Assign original class
    class(x) <- class(vec)
    # Return vector
    x
  }
}
