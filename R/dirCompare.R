#' Recursively compare two directories
#'
#' @param dir1 directory to compare
#' @param dir2 directory to compare
#' @param Recursive logical to perform recursive comparison
#' @param dir1.excludes character vector of files to exclude
#' @param compareAttributes logical to compare file attributes
#' @export
dirCompare <- function(dir1, dir2, Recursive = FALSE,
                       dir1.excludes = NULL, compAttributes = FALSE){
  if(FALSE){
    dir1 <- "l:/Lab/Lablan/MSE/Lamba35/SpecFiles/"
    dir2 <- "l:/Lab/Lablan/MSE/Lamba35/SpecFiles/Archive/"
  }
  # If directories exist get contents
  if(!dir.exists(dir1)){
    warning(paste("Directory dir1 does not exist:", dir1))
    return(FALSE)
  }else{
    dir1_ <-
      tibble::as.tibble(list.files(dir1, recursive = Recursive,
                                   all.files = TRUE, full.names = FALSE)) %>%
      dplyr::rename(FilePath = value) %>%
      dplyr::filter(!grepl(dir1.excludes, FilePath)) %>%
      dplyr::mutate(FileName = basename(FilePath)) #%>% #slice(1) %>%
    # Get info - this is slow
    if(compAttributes){
      dir1_ %<>%
        dplyr::rowwise() %>%
        dplyr::bind_cols(file.info(file.path(dir1, .$FilePath))[, c("size", "mtime")]) %>%
        dplyr::ungroup()
    }
  }

  if(!dir.exists(dir2)){
    warning(paste("Directory dir2 does not exist:", dir2))
    return(FALSE)
  }else{
    dir2_ <-
      tibble::as.tibble(list.files(dir2, recursive = Recursive,
                                   all.files = TRUE, full.names = FALSE)) %>%
      dplyr::rename(FilePath = value) %>%
      dplyr::mutate(FileName = basename(FilePath)) #%>%
    # Get info
    if(compAttributes){
      dir2_ %<>%
        dplyr::rowwise() %>%
        dplyr::bind_cols(file.info(file.path(dir2, .$FilePath))[, c("size", "mtime")]) %>%
        dplyr::ungroup()
    }
  }

  # Compare
  fDif <- anti_join(dir1_, dir2_)
  print(paste(nrow(dir1_), "files in", dir1))
  print(paste(nrow(dir2_), "files in", dir2))
  if(is.null(fDif)){
    print("All files the same")
  }else{
    fDif %>%
      dplyr::select(FilePath, FileSize = size, LastModified = mtime)
  }
}
