# Functions to summarize data

#' Coefficient of variation
#'
#' Calculate coefficient of variation of vector
#' @param x vector
#' @param ... parameters to be passed to sd and mean
#' @export
cv <- function(x,...){
  (sd(x, ...)/mean(x, ...)) * 100
}

#' Data Summary Stats
#'
#' Function used by summaryBy or summarise to compute summary stats on grouped data.
#'
#'  Returns N, Min, Max, Lower Quartile, Median, Upper Quartile, Max, Mean, Standard Deviation, Standard Error of the Mean, Coefficient of Variation, and mean confidence intervals.
#' @param x numeric vector
#' @param confInt numeric value giving p value for confidence intervals. If NULL, none are calculated.
#' @param ... parameters to be passed to summary functions such as na.rm or na.omit
#' @export
sem <- function(x, confInt = 0.05, ...){
  # Remove NAs
  x_ <- na.omit(x)

  # Calc sigfigs of SD
  SD <- signif(sd(x_, ...), 2)
  if(is.na(SD)){
    sigFig <- 1
  } else if(SD - trunc(SD) == 0){
    sigFig <- 0
  } else{
    sigFig <- nchar(sub('.*\\.', '', SD - trunc(SD)))
  }

  # Calculate Five number summary
  fiveNum <- fivenum(x_)

  # Generate summary
  tibble::tibble(
  N = length(x_),
  N.NA = N - length(x),
  Min = fiveNum[1],
  lHinge  = round(fiveNum[2], sigFig),
  Median = fiveNum[3],
  uHinge = round(fiveNum[4], sigFig),
  Max = fiveNum[5],
  SD = round(sd(x_, ...), sigFig),
  Mean = round(mean(x, ...), sigFig),
  SEM = round(SD / sqrt(N), sigFig), # could also be calculated sqrt(var(x)/N)
  CV = round(SD / Mean * 100, 2),
  l95 = round(Mean - qnorm(1 - confInt / 2) * SEM, sigFig),
  u95 = round(Mean + qnorm(1 - confInt / 2) * SEM, sigFig)
  )
}

#' Calculate Relative Percent Difference
#'
#' @param x vector of length 2
#' @export
RPD <- function(x){
  round((abs(diff(x)) / mean(x)) * 100, 2)
}
