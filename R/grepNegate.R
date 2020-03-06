#' Function to apply negative regex in grep
#'
#' @param neg character string to be negated
#' @param string character string to search
#' @param value passed to grep. default is true
#' @param ... parameters to be passed to grep
#' @export
grepN <- function(neg, string, value = TRUE, ...){
  if(FALSE){
    neg <- "abc"
    string <- c("abc", "def")
  }
  regex <- paste0("^((?!", neg, ").)*$")
  grep(regex, string, perl = TRUE, value = value, ...)


}

#' Function to apply negative regex in grepl
#'
#' @param neg character string to be negated
#' @param string character string to search
#' @param ... parameters to be passed to grep
#' @export
greplN <- function(neg, string, ...){
  if(FALSE){
    neg <- "abc"
    string <- c("abc", "def")
  }
  regex <- paste0("^((?!", neg, ").)*$")
  grepl(regex, string, perl = TRUE, ...)


}
