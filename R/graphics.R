#' This function dynamically produces a knitr chunk with appropriate captions and labels which is then knitted into the main document with cat.  Any chunk option could be added for full control over chunk.
#' @param chunkLabel text for chunk label which is also used for figure file name
#' @param capt text for caption
#' @param plt plot object to be placed
#' @param pos latex option for keeping the plot here
#' @export
plotKknit <- function(chunkLabel, capt, plt, pos='H', ...)
{
  cat(
    knitr::knit(
      text =
        knitr::knit_expand(
          text = "<<{{chunkLabel}},eval=TRUE,fig.pos='{{pos}}',fig.cap='{{capt}}'>>=\nplt\n@")
    )
  )
}

#' Plot handler
#'
#' Function to save and display plots
#' @param visPlot logical to display plot
#' @param makeObject logical to save plots in environment
#' @param savePlots logical to save plots as image
#' @param filename filename to pass to ggsave_f
#' @param path path where plots should be saved to pass to ggsave_f
#' @param plt plot to be printed to pass to ggsave_f
#' @param device list of formats to be saved to pass to ggsave_f
#' @param dpi image resolution to pass to ggsave_f
#' @export
plotHandler <- function(visPlot = FALSE, makeObject = FALSE, savePlots = TRUE,
                        filename = NULL, path = NULL,
                        plt = ggplot2::last_plot(), device = c("pdf", "png", "wmf"),
                        dpi = 300){
  if(visPlot){
    if(inherits(plt, "plotly")){
      print(plt)
    } else if(any(grepl("gTree", class(plt)))){
      grid::grid.newpage()
      grid::grid.draw(plt)
    } else plot(plt)
  }
  if(makeObject) assign(paste0(filename, "_plot"), plt, envir = .GlobalEnv)
  if(savePlots)
    tryCatch(ggsave_f(filename = filename, filepath = path, plt = plt, device = device, dpi = dpi),
             error = function(e) e)
}

#' Arrange multiple ggplots with single legend
#'
#' Take a list of plots, arrange in a grid with a title
#' @param ... plots
#' @param plots list of plots
#' @param ncolN number of columns for plots grob
#' @param nrowN number of rows for plots grob
#' @param layOut matrix for layout of plots
#' @param position where should the legend be
#' @param Title optional title
#' @param TitleJust Title justification 0 = right, 0.5 = center, 1 = left
#' @param TitleSize Title size in points
#' @export
grid_arrange_shared_legend <-
  function(..., plots = NULL, colN = NULL, rowN = NULL,
           layOut = NULL,
           position = c("bottom", "right"),
           Title = NULL,
           TitleJust = 1,
           TitleSize = 18) {

    if(is.null(plots)){
      plots <- list(...)
    }

    if(is.null(layOut)){
      if(is.null(colN)){
        colN = length(plots)
        # print(paste("ncol =", ncol))
      }
      if(is.null(rowN)){
        rowN <- ceiling(length(plots) / colN)
        # print(paste("nrow =", nrow))
      }
    }
    # print(paste("n plots =", length(plots), "Row x Col =", nrow * ncol))

    position <- match.arg(position, c("bottom", "right"))
    # Extract grobs from first plot
    g <-
      ggplot2::ggplotGrob(
        plots[[1]] +
          ggplot2::theme(legend.position = position))$grobs
    # Extract legend grob from g
    legend <-
      g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    # Get height and width
    lheight <-
      sum(legend$height)
    lwidth <-
      sum(legend$width)
    # Remove legend from plots
    gl_ <-
      lapply(plots, function(x) x + ggplot2::theme(legend.position="none"))
    # Parameters list for do.call
    if(!is.null(layOut)){
      gl_$layout_matrix <- Layout
    } else  gl_ <- c(gl_, ncol = colN, nrow = rowN)

    # print(str(Layout))
    # print(gl_$layout_matrix)
    # gl_<<- gl_
    # Create combined plot
    combined <- switch(position,
                       "bottom" =
                         gridExtra::arrangeGrob(
                           do.call(gridExtra::arrangeGrob, gl_),
                           legend,
                           ncol = 1,
                           heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)
                         ),
                       "right" =
                         gridExtra::arrangeGrob(
                           do.call(grid::arrangeGrob, gl_),
                           legend,
                           ncol = 2,
                           widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)
                         )
    )

    # Add Title
    if(!is.null(Title)){
      combined <-
        gridExtra::arrangeGrob(
          grid::textGrob(Title,
                         gp = grid::gpar(fontsize = TitleSize),
                         x = grid::unit(TitleSize, "points"),
                         hjust = TitleJust#,
                         # just = "left"
          ),
          combined,
          ncol = 1,
          heights = grid::unit.c(
            grid::unit(TitleSize, "points"),
            grid::unit(1, "npc") - grid::unit(TitleSize, "points"))
        )
    }

    # return gtable invisibly
    invisible(combined)
  }

#' Custom ggsave function
#'
#' Allows for multiple file formats
#'
#' If verbose, return vector of filenames created else nothing
#' @param filename filename
#' @param path path where plots should be saved
#' @param plt plot to be printed
#' @param device vector of graphic device(s) for output. pdf and png use Cairo.
#' @param perspective vector of perspectives to plot
#' @param pWidth plot width
#' @param Units pWidth units
#' @param dpi image resolution
#' @param verbose logical to provide feedback
#' @export
ggsave_f <-
  function(filename,
           filepath = NULL,
           plt = ggplot2::last_plot(),
           device = c("pdf", "png", "wmf"),
           perspective = c("4.3", "16.9"),
           pWidth = 6,
           Units = "in",
           dpi = 300,
           verbose = F) {
    # Check filename
    if(!exists("filename")){
      message("filename not provided")
      return(NULL)
    }

    # if path is NULL then add
    if(is.null(filepath)){
      filepath <- path$wd_plots
    }

    # Set filenames
    filename <-
      paste(file.path(filepath, filename),
            # Set extensions
            apply(
              expand.grid(
                perspective,
                sapply(device, USE.NAMES = F,
                       switch,
                       pdf = "pdf",
                       png = "png",
                       wmf = "wmf")
              ), 1, paste, collapse="."),
            sep = "_")

    # Calc height based on perspective
    pHeight <-
      sapply(perspective,
             function(x){
               x <- as.numeric(strsplit(x, "\\.")[[1]])
               pWidth / (x[-length(x)]/x[-1])
             }, USE.NAMES = FALSE)

    # If pdf in devices, use cairo
    device[grepl("pdf", device)] <- "cairo_pdf"

    # Set fileinfo
    plotinfo <-
      data.frame(filename = filename,
                 pWidth = pWidth,
                 pHeight = pHeight,
                 Units = Units,
                 dpi = dpi,
                 device = as.character(expand.grid(perspective, device)[,2]),
                 stringsAsFactors = FALSE
      )


    # Save plots
    for(x in 1:nrow(plotinfo)){
      do.call(ggplot2::ggsave, list(
        filename = plotinfo$filename[x],
        plot = plt,
        width = plotinfo$pWidth[x],
        height = plotinfo$pHeight[x],
        units = plotinfo$Units[x],
        dpi = plotinfo$dpi[x],
        device =  ifelse(grepl("(?i)cairo", plotinfo$device[x]),
                         get(plotinfo$device[x]),
                         plotinfo$device[x]),
        type = "cairo-png")[c(T, T, T, T, T, T, T,
                              grepl("(?i)png", plotinfo$device[x]))])
    }

    # Return filenames
    if(verbose) return(filename)
  }

#' Create layouts for grid allowing for stacking of plots
#'
#' @param n number of total plots
#' @param ncol_ max number of columns
#' @param stack number of plots which should be stacked
#' @export
LayoutHandler <-
  function(n, col_ = 6, stack = 2){
    ncol_ <- ifelse(n/stack > col_, col_, n/stack)
    nrow_ <- floor(n/(ncol_ * stack))
    Layout <- NULL
    for(c_ in seq(1, n, ncol_*2)){
      for(n_ in 0:1){
        Layout <- rbind(Layout,
                        seq(1, n)[seq(c_ + n_, by = 2, length.out = ncol_)]
        )
      }
    }
    Layout
  }

#' Function to parse linear models for adding to plot
#' @param lm lm object
#' @export
lm_eqn <- function(m) {

  l <- list(a = format(stats::coef(m)[1], digits = 2),
            b = format(abs(stats::coef(m)[2]), digits = 4),
            r2 = format(summary(m)$r.squared, digits = 3))

  if (stats::coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }
  as.character(as.expression(eq));
}

#' Function to parse linear models for adding to plot
#' @param lm lm object
#' @export
lm_eqn2 <- function(x) {
  m <- stats::lm(stats::as.formula(paste0(names(x)[2], '~', names(x)[1])), data = x)
  l <- list(a = format(stats::coef(m)[1], digits = 2),
            b = format(abs(stats::coef(m)[2]), digits = 4),
            r2 = format(summary(m)$r.squared, digits = 3))

  if (coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }
  as.character(as.expression(eq));
}

#' Workaround to suppress unwanted readxl output
#' @export
read_excelq <-  function(...) {
  quiet_read <- purrr::quietly(readxl::read_excel)
  out <- quiet_read(...)
  if(length(c(out[["warnings"]], out[["messages"]])) == 0)
    return(out[["result"]])
  else readxl::read_excel(...)
}

#' #' Function to modify ggplot facet_grid panel labels
#' #' @export
#' plot_labeller <- function(variable,value){
#'   return(plot_names[value])
#' }
