#' Create and format flextable
#'
#' Similar to cat_ptr but uses flextable for word documents
#'
#' @param df data frame for table
#' @param Digits number of decimal places for rounding
#' @param Caption table caption
#' @param Tag tag for pandoc-crossref
#' @param noHeader logical to use header
#' @param theme_ flextable theme to apply
#' @param first.row.names use first row as column headers
#' @param row.names.null use row names
#' @param parse.pattern pattern to replace
#' @param parse.replace string to replace parse.pattern
#' @param font_ font to use
#' @param fontsizeH header font size in pts
#' @param fontsizeB body font size in pts
#' @param rotationH logical vector of header columns to rotate
#' @param rotationB logical vector of body columns to rotate
#' @param alignB body cell text alignment
#' @param formatter function for formatting cells
#' @export
cat_ft <- function(df, Digits = 3, Caption = NULL,
                   Tag = NULL, noHeader = FALSE,
                   theme_ = "theme_vanilla",
                   first.row.names = FALSE, row.names.null = TRUE,
                   parse.pattern = '_', parse.replace = ' ',
                   font_ = "Cambria", fontsizeH = 8, fontsizeB = 8,
                   rotationH = NULL, rotationB = NULL,
                   alignB = "left", formatter = NULL
)
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
             tz = lubridate::tz(df[,date.test]))
  }

  #test for logical class columns in df and convert to character
  logical.test <- grepl('logi',lapply(df,class))
  if(any(logical.test)){
    df[,logical.test] <-
      as.character(df[,logical.test])
  }

  # replace parse.pattern with parse.replace in colnames
  # if(!is.null(parse.pattern)){
  #   colnames(df) <- gsub(parse.pattern, parse.replace, colnames(df))
  # }
  # print(which(names(df) %in% rotationH))
  # Create Table
  dfT <-
    df %>%
    flextable::regulartable() %>%
    # Set style for header row
    flextable::style(
      pr_c = officer::fp_cell(
        vertical.align = "bottom",
        border.bottom = officer::fp_border(width = 2)),
      part = "header") %>%
    # Set style for body row
    flextable::style(
      pr_c = officer::fp_cell(
        vertical.align = "bottom",
        border.bottom = officer::fp_border(width = 2),
        margin = 0.9),
      part = "body") %>%
    flextable::font(fontname = font_, part = "all") %>%
    flextable::fontsize(size = fontsizeH, part = "header") %>%
    flextable::fontsize(size = fontsizeB, part = "body") %>%
    flextable::align(align = alignB, part = "body")

  theme_ <- get(theme_, asNamespace("flextable"))
  # dfT <- flextable::theme_vanilla(dfT)
  dfT <- do.call(theme_, list(x = dfT))

  if(!is.null(rotationH)){
    dfT %<>%
      flextable::rotate(j = rotationH,
                        rotation = "tbrl", part = "header", align = "bottom") %>%
      flextable::align(j = names(df)[!names(df) %in% rotationH],
                       align = "left", part = "header") %>%
      flextable::height(height = max(dim_pretty2(., part = "header")$widths),
                        part = "header") %>%
      flextable::width(width =
                         c(dim_pretty2(. , part = "all")$widths[which(names(df) %in%
                                                                                  rotationH)],
                           dim_pretty2(. , part = "body")$widths[!which(names(df) %in%
                                                                                    rotationH)]))
  }else{
    dfT %<>%
      flextable::align(align = alignB, part = "body") %>%
      flextable::height(height =
                          max(dim_pretty2(., part = "header")$heights),
                        part = "header") %>%
      flextable::width(width = dim_pretty2(., part = "all")$widths)
    # print(dim_pretty2(dfT, part = "all")$widths)
    # print(dim_pretty2(dfT, part = "header")$widths)
    # print(dim_pretty2(dfT, part = "body")$widths)
  }

  if(!is.null(rotationB)){
    dfT <- flextable::rotate(dfT, j = rotationB,
                             rotation = "tbrl", part = "body", align = "bottom")
  }
  if(!is.null(formatter)){
    dfT <- flextable::set_formatter(dfT, formatter)
  }
  if(noHeader){
    dfT <- flextable::delete_part(dfT)
  }

  # dfT <- flextable::autofit(dfT)

  # theme_ <- get(theme_, asNamespace("flextable"))
  # # dfT <- flextable::theme_vanilla(dfT)
  # dfT <- do.call(theme_, list(x = dfT))

  # Write out with crossref wrapping
  # cat(sprintf("<div id=\"fig:%s\">    ", Tag))
  cat(paste0("<div id=\"tbl:", Tag, "\">"))
  # flextable:::knit_print.flextable(dfT)

  # cat("    </div>")
  # dfT
  cat("```{=openxml}\n")
  cat(flextable:::docx_str.flextable(dfT), "\n")
  # cat(flextable:::docx_str.regulartable(dfT), "\n")
  # paste0(cat(Caption,"\n\n"))
  cat("```\n", Caption, "\n</div>\n")
}

#' @export
dim_pretty2 <- function(x, part = "all", linefeed = "\n"){
  part <- match.arg(part, c("all", "body", "header", "footer"),
                    several.ok = FALSE)
  if ("all" %in% part) {
    part <- c("header", "body", "footer")
  }
  dimensions <- list()
  for (j in part) {
    if (flextable:::nrow_part(x, j) > 0) {
      dimensions[[j]] <- #optimal_sizes_simple_tabpart(x[[j]])
        flextable:::optimal_sizes(x[[j]])
    }
    else {
      dimensions[[j]] <- list(widths = rep(0, length(x$col_keys)),
                              heights = numeric(0))
    }
  }
  widths <- lapply(dimensions, function(x) x$widths)
  widths <- as.numeric(apply(do.call(rbind, widths), 2, max,
                             na.rm = TRUE)) * 1.1
  heights <- lapply(dimensions, function(x) x$heights)
  heights <- as.numeric(do.call(c, heights))
  list(widths = widths, heights = heights)
}

#' @export
optimal_sizes_simple_tabpart <- function( x ){

  txt_data <- flextable:::get_text_data(x)
  txt_data$type_out <- rep("text", nrow(txt_data))
  txt_data$pos <- rep(1, nrow(txt_data))
  text_fp <- x$styles$text$get_fp()

  sizes <- flextable:::text_metric(data = txt_data, all_fp = text_fp)
  ### START new code
  # Rerun with max strsplit(txt_dat$str, "\n")[[1]]
  txt_data$str <-
    sapply(txt_data$str,
           function(y){
             s_ <- strsplit(y, "\n")[[1]]
             ifelse(length(s_ == 1), s_, s_[which.max(nchar(s_))])
           },
           USE.NAMES = FALSE)
  # Recalc width
  sizes$width <- flextable:::text_metric(data = txt_data, all_fp = text_fp)$width
  ### END new code
  sizes$col_key <- factor(sizes$col_key, levels = x$col_keys)
  sizes <- sizes[order(sizes$col_key, sizes$id ), ]
  widths <- flextable:::as_wide_matrix_(as.data.frame(sizes[, c("col_key", "width", "id")]))
  heights <- flextable:::as_wide_matrix_(as.data.frame(sizes[, c("col_key", "height", "id")]))

  par_dim <- flextable:::dim_paragraphs(x)
  widths <- widths + par_dim$widths
  heights <- heights + par_dim$heights

  widths[x$spans$rows<1] <- 0
  widths[x$spans$columns<1] <- 0
  heights[x$spans$rows<1] <- 0
  heights[x$spans$columns<1] <- 0

  cell_dim <- flextable:::dim_cells(x)
  widths <- widths + cell_dim$widths
  heights <- heights + cell_dim$heights
  list(widths = apply(widths, 2, max, na.rm = TRUE),
       heights = apply(heights, 1, max, na.rm = TRUE)
  )
}
