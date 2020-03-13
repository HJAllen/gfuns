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
