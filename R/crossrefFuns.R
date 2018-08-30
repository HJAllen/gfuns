#' Generalized function to insert multiple figure or table references
#'
#' Concatenates multiple figure references for pandoc-crossref based on multiFig and multiTbl functions.
#' @param pre pre tag string
#' @param post post tag string
#' @param ref reference type - fig, tbl, etc
#' @param s ref or tag
#' @param sep separator for paste
#' @export
multiRef <-
  function(pre = NULL, post = NULL, ref = NULL, s = "ref", sep = "."){
    if(s == "ref"){
      paste0(paste0("[@", ref, ":", pre, sep, post,"]"), collapse = ";")
    }else if(s == "tag"){
      paste0(paste0("{#", ref, ":", pre, sep, post,"}"), collapse = ";")
    }
  }

#' Concatenate multiple figure references
#'
#' Uses pandoc-crossref style
#' @param figpre pre tag string
#' @param figpost post tag string
#' @param sep paste separator
#' @export
multiFig <-
  function(figpre = NULL, figpost, sep = "."){
    paste0(paste0("@fig:", figpre, sep,
                  figpost), collapse = ";")
  }

#' Concatenate multiple table references
#'
#' Uses pandoc-crossref style
#' @param figpre pre tag string
#' @param figpost post tag string
#' @param sep paste separator
#' @export
multiTbl <-
  function(figpre = NULL, figpost, sep = "."){
    paste0(paste0("@tbl:", figpre, sep,
                  figpost), collapse = ";")
  }
