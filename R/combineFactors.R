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
