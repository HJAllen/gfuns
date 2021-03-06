% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panderFuns.R
\name{cat_pir3}
\alias{cat_pir3}
\title{modification of cat.pir2 to allow for resizing using magick package
pixels in a letter page = 612 x 792 so pixels per inch = 612/8.5 = 72
To use function like map, caption and tag are split into pre and post elements and glued using multFig and multiTbl}
\usage{
cat_pir3(
  plot_g,
  Caption = NULL,
  TagPre = NULL,
  TagPost = NULL,
  Width = knitr::opts_chunk$get("fig.width"),
  Height = knitr::opts_chunk$get("fig.height"),
  vec.res = 300
)
}
\arguments{
\item{plot_g}{plot to be added to document}

\item{Caption}{caption for plot}

\item{TagPre}{string to begin tag to be referenced using pandoc-crossref}

\item{TagPost}{string to end to be referenced using pandoc-crossref}

\item{Width}{figure width}

\item{Height}{figure width}

\item{vec.res}{figure resolution}
}
\description{
modification of cat.pir2 to allow for resizing using magick package
pixels in a letter page = 612 x 792 so pixels per inch = 612/8.5 = 72
To use function like map, caption and tag are split into pre and post elements and glued using multFig and multiTbl
}
