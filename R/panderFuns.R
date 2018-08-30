#' Create pandoc table
#' @param df data frame for table
#' @param Digits number of decimal places for rounding
#' @param Caption table caption
#' @param Justify text justification
#' @param Style markdown table style
#' @param Split.table number of rows to split table across pages
#' @param Split.cells number of cells to splist table
#' @param Keep.line.breaks keep line breaks in cells
#' @param Tag tag for pandoc-crossref
#' @param font.s latex font size
#' @param first.row.names use first row as column headers
#' @param row.names.null use row names
#' @param parse.pattern pattern to replace
#' @param parse.replace string to replace parse.pattern
#' @param Latex is document latex
#' @export
cat_ptr <- function(df, Digits = 3, Caption = NULL, Justify='left',
                    Style = 'simple', Split.table = Inf, Split.cells = Inf,
                    Keep.line.breaks = TRUE,
                    Tag = NULL, font.s = '\\footnotesize\n',
                    first.row.names = FALSE, row.names.null = TRUE,
                    parse.pattern = '_', parse.replace = ' ',
                    Latex = TRUE)
{
  if(first.row.names){
    df.names <- as.character(df[1,])
    df <- as.data.frame(df[-1,])
    colnames(df) <- df.names
  }

  #test for POSIX class columns in df so non-breaking hyphens can be inserted
  date.test <- grepl('POSIX',lapply(df,class))
  if(any(date.test)){
    df[,date.test] <-
      format(df[,date.test],
             format = '%Y\\-%m\\-%d',
             tz = tz(df[,date.test]))
  }

  # Make changes suitable for latex
  if(Latex){
    # test for columns with < or > sign
    if(any(grepl('[<>]', df))){
      df <-
        tibble::as_tibble(llply(
          as.list(df),
          .fun = function(x){
            x <- sub('<', replacement = '\\\\textless ', x)
            sub('>', replacement = '\\\\textgreater ', x)
          }
        )
        )
    }
  }

  # replace parse.pattern with parse.replace in colnames
  if(!is.null(parse.pattern)){
    colnames(df) <- gsub(parse.pattern, parse.replace, colnames(df))
  }

  if(row.names.null){
    row.names(df) <-NULL
  }

  if(Latex){
    cat(font.s)
  }

  # Create Table
  pander::pandoc.table(df,
                       digits = Digits,
                       caption = paste(Caption, Tag),
                       justify = Justify,
                       style = Style,
                       split.table = Split.table,
                       split.cells = Split.cells,
                       keep.line.breaks = Keep.line.breaks
  )

  if(Latex){
    cat('\n\n\\normalsize\n\n')
    # cat("\\FloatBarrier","\n\n")
  }
}

#' Format and add image using pander::pandoc.image.return
#'
#' Submitted plot is added to document with tags
#' @param plot_g plot to be added to document
#' @param Caption caption for plot
#' @param Tag tag to be referenced using pandoc-crossref
cat_pir <- function(plot_g, Caption = NULL, Tag = NULL)
{
  cat(pander::pandoc.image.return(plot_g, caption = Caption),
      Tag,
      "\n\n", sep='')
  # cat("\\FloatBarrier","\n\n")
}

#' Format and add image using pander::pandoc.image.return
#'
#' Modification of cat.pir2 to allow for resizing using magick package. Pixels in a letter page = 612 x 792 so pixels per inch = 612/8.5 = 72
#' @param plot_g plot to be added to document
#' @param Caption caption for plot
#' @param Tag tag to be referenced using pandoc-crossref
#' @param Width figure width
#' @param Height figure width
#' @param vec.res figure resolution
#' @export
cat_pir2 <- function(plot_g, Caption = NULL, Tag = NULL,
                     Width = knitr::opts_chunk$get('fig.width'),
                     Height = knitr::opts_chunk$get('fig.height'),
                     vec.res = 300)
{
  func.env <- new.env()
  # handle ggplot objects
  if(ggplot2::is.ggplot(plot_g)){
    # convert wxh into pixels
    Width <- Width * vec.res
    Height <- Height * vec.res
    plot.return <- pander::evals('plot_g', res = vec.res, env = func.env,
                                 width = Width, height = Height)[[1]]$result
  }else if(file.exists(plot_g)){ # handle image files
    if(grepl('pdf',plot_g)){
      plot.i <- magick::image_read(plot_g, density = vec.res, depth = 8)
    }else{
      plot.i <- magick::image_read(plot_g)
    }
    # set height
    Height = as.character(72 * Height)
    Width = as.character(72 * Width)
    if(is.null(Width)){
      magick::image_scale(plot.i, paste0('x', Height))
    }else if(is.null(Height)){
      magick::image_scale(plot.i, paste0(Width, 'x'))
    }else if(!is.null(Width) & !is.null(Height)){
      magick::image_scale(plot.i, paste0(Width, 'x', Height))
    }
    magick::image_write(plot.i, plot_g)
    plot.return <- plot_g
  }
  cat(pander::pandoc.image.return(plot.return, caption = Caption),
      Tag,
      "\n\n", sep='')
  # cat("\\FloatBarrier","\n\n")
}

#' modification of cat.pir2 to allow for resizing using magick package
#' pixels in a letter page = 612 x 792 so pixels per inch = 612/8.5 = 72
#' To use function like map, caption and tag are split into pre and post elements and glued using multFig and multiTbl
#' @param plot_g plot to be added to document
#' @param Caption caption for plot
#' @param TagPre string to begin tag to be referenced using pandoc-crossref
#' @param TagPost string to end to be referenced using pandoc-crossref
#' @param Width figure width
#' @param Height figure width
#' @param vec.res figure resolution
#' @export
cat_pir3 <- function(plot_g, Caption = NULL, TagPre = NULL,
                     TagPost = NULL, Width = knitr::opts_chunk$get('fig.width'),
                     Height = knitr::opts_chunk$get('fig.height'),
                     vec.res = 300)
{
  func.env <- new.env()
  # handle ggplot objects
  if(ggplot2::is.ggplot(plot_g)){
    # convert wxh into pixels
    Width <- Width * vec.res
    Height <- Height * vec.res
    plot.return <- pander::evals('plot_g', res = vec.res, env = func.env,
                                 width = Width, height = Height)[[1]]$result
  }else if(file.exists(plot_g)){ # handle image files
    if(grepl('pdf',plot_g)){
      plot.i <- magick::image_read(plot_g, density = vec.res, depth = 8)
    }else{
      plot.i <- magick::image_read(plot_g)
    }
    # set height
    Height = as.character(72 * Height)
    Width = as.character(72 * Width)
    if(is.null(Width)){
      magick::image_scale(plot.i, paste0('x', Height))
    }else if(is.null(Height)){
      magick::image_scale(plot.i, paste0(Width, 'x'))
    }else if(!is.null(Width) & !is.null(Height)){
      magick::image_scale(plot.i, paste0(Width, 'x', Height))
    }
    magick::image_write(plot.i, plot_g)
    plot.return <- plot_g
  }
  Caption <- paste(Caption, collapse = " ")
  Tag <- gfuns::multiRef(pre = TagPre, post = TagPost, ref = "fig", s = "tag")
  cat(pander::pandoc.image.return(plot.return, caption = Caption),
      Tag,
      "\n\n", sep='')
  # cat("\\FloatBarrier","\n\n")
}
