#' Convert utf8 hex to character
#'
#' Wrapper for rawToChar to place utf8 symbols in text
#' @param x hex or character vector of utf8 character codes
#' @examples
#' # Character codes can be found at https://www.utf8-chartable.de/
#' plusminus <- "0xb1"
#' tilde <- "0x7e"
#' # Codes don't have to be quoted
#' atsign <- 0x40
#' degreeSymbol <- 0xb0
#' asterisk <- 0x2a
#' u2c(plusminus)
#' # Vector
#' u2c(c(0x48, 0x65, 0x6c, 0x6c, 0x6f))
#' @export
u2c <- function(x){
  rawToChar(as.raw(x))
}

#' Clean up unicode characters
#'
#' @param x character string
#' @param specials character matrix where col1 is character to replace and col2 is replacement character
#' @param csv is file a csv
#' @examples
#' # bo = degree sign
#' # b5 = greek mu
#' @export
unicodeClean <- function(x, specials = matrix(c("b5", "u"), ncol = 2), csv = FALSE){
  # print(x)
  # Strip trailing \t
  x <- gsub("\\t$", "", x)
  # If csv convert , to \t
  if(csv)x <- gsub(",", "\\\t", x)
  #convert to raw
  x_ <- charToRaw(x)
  # Remove trouble makers
  # bo = degree sign
  # b5 = greek mu
  # c2 = goofy A
  # ff and fe characters at beginning
  x_ <- x_[!grepl("ff|fe|b0|c2", x_, perl = T)]
  # Convert specials
  if(!is.null(specials)){
    for(i in 1:nrow(specials)){
      x_[grepl(specials[i], x_)] <- charToRaw(specials[i + 1])
    }
  }
  # Convert back to character
  ifelse(!is.null(x_), rawToChar(x_), x)
}
