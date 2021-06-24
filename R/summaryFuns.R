# Functions to summarize data

#' Coefficient of variation
#'
#' Calculate coefficient of variation of vector
#' @param x vector
#' @param sigFig if not null, the number of significant digits to report
#' @param ... parameters to be passed to sd and mean
#' @export
cv <- function(x, sigFig = NULL, ...){
  # make sure x is length 2
  if(length(x) != 2){
    stop("gfuns::cv: length(x) != 2")
  }
  # If all equal, return 0
  if(identical(x[1], x[2])) return(0)

  if(is.null(sigFig)){
    (stats::sd(x, ...)/mean(x, ...)) * 100
  } else {
    signif(
      (stats::sd(x, ...)/mean(x, ...)) * 100,
      digits = sigFig)
  }
}

#' Calculate Relative Percent Difference
#'
#' @param x vector of length 2
#' @param sigFig if not null, the number of significant digits to report
#' @export
RPD <- function(x, sigFig = NULL){
  # make sure x is length 2
  if(length(x) != 2){
    stop("gfuns::RPD: length(x) != 2")
  }
  # If all equal, return 0
  if(identical(x[1], x[2])) return(0)

  if(is.null(sigFig)){
    abs(diff(x)) / mean(x) * 100
  } else {
    signif(
      abs(diff(x)) / mean(x) * 100,
      digits = sigFig)
  }
}

#' Data Summary Stats
#'
#' Function used by summaryBy or summarise to compute summary stats.
#'
#'  Returns N, Min, Max, Lower Quartile, Median, Upper Quartile, Max, Mean, Standard Deviation, Standard Error of the Mean, Coefficient of Variation, and mean confidence intervals.
#' @param x numeric vector
#' @param confInt numeric value giving p value for confidence intervals. If NULL, none are calculated.
#' @param sigFig number of significant digits to return, if NULL, it is calculated based on standard deviation
#' @param ... parameters to be passed to summary functions such as na.rm or na.omit
#' @export
sem <- function(x, confInt = 0.05, sigFig = NULL, ...){
  # Remove NAs
  x_ <- stats::na.omit(x)

  # Calc sigfigs of SD
  if(is.null(sigFig)){
    SD <- signif(stats::sd(x_, ...), 2)
    if(is.na(SD)){
      sigFig <- 1
    } else if(SD - trunc(SD) == 0){
      sigFig <- 0
    } else{
      sigFig <- nchar(sub('.*\\.', '', SD - trunc(SD)))
    }
  }

  # Calculate Five number summary
  fiveNum <- stats::fivenum(x_)

  # Generate summary
  tibble::tibble(
    N = length(x_),
    N.NA = N - length(x),
    Min = fiveNum[1],
    lHinge  = round(fiveNum[2], sigFig),
    Median = fiveNum[3],
    uHinge = round(fiveNum[4], sigFig),
    Max = fiveNum[5],
    SD = round(stats::sd(x_, ...), sigFig),
    Mean = round(mean(x, ...), sigFig),
    SEM = round(SD / sqrt(N), sigFig), # could also be calculated sqrt(var(x)/N)
    CV = round(SD / Mean * 100, 2),
    # Use the t distribution rather than normal distribution to calculate conf intervals
    t95 = round(stats::qt(1 - confInt / 2, df = N - 1), 2),
    l95 = round(Mean - t95 * SEM, sigFig),
    u95 = round(Mean + t95 * SEM, sigFig)
    # l95 = round(Mean - stats::qnorm(1 - confInt / 2) * SEM, sigFig),
    # u95 = round(Mean + stats::qnorm(1 - confInt / 2) * SEM, sigFig)
  )
}

#' Data Summary Stats for HarshaHabs data
#'
#' Function used to consistently return summary data for inclusion in HarshaHabs databse.
#'
#'  Returns N, Min, Max, Lower Quartile, Median, Upper Quartile, Max, Mean, Standard Deviation, Standard Error of the Mean, Coefficient of Variation, and mean confidence intervals.
#' @param x numeric vector
#' @param confInt numeric value giving p value for confidence intervals. If NULL, none are calculated.
#' @param sigFig number of significant digits to return, if NULL, it is calculated based on standard deviation
#' @param ... parameters to be passed to summary functions such as na.rm or na.omit
#' @export
HHsummary <- function(x, confInt = 0.05, sigFig = NULL, ...){
  # Observation number
  N <- length(x)
  # Number of NA observations
  NA_N <- sum(is.na(x))
  # Remove NAs
  x_ <- x#stats::na.omit(x)


  # Standard Devation
  SD <- signif(stats::sd(x_), 2)
  # Calc sigfigs of SD
  if(is.null(sigFig)){

    if(is.na(SD)){
      sigFig <- 1
    } else if(SD - trunc(SD) == 0){
      #
      sigFig <- 0
    } else{
      sigFig <- nchar(sub('.*\\.', '', SD - trunc(SD)))
    }
  }

  # Calculate Five number summary
  fiveNum <- stats::fivenum(x_)
  # fiveNum <- stats::setNames(stats::fivenum(x_),
  #                            c("Min", "LQuartile", "Median", "UQuartile", "Max"))
  # Calc mean
  Mean <- round(mean(x_), sigFig)

  # Generate summary
  data.frame(
    N = N,
    NA_N = NA_N,
    Min = fiveNum[1],
    LQuartile  = round(fiveNum[2], sigFig),
    Median = fiveNum[3],
    UQuartile = round(fiveNum[4], sigFig),
    Max = fiveNum[5],
    Mean = Mean,
    SD = round(stats::sd(x_), sigFig),
    SEM = round(SD / sqrt(N), sigFig),
    # Use the t distribution rather than normal distribution to calculate conf intervals
    t95 = round(stats::qt(1 - confInt / 2, df = N - 1), 2),
    CV = round(SD / Mean * 100, 2)
    ) %>%
    data.table::as.data.table()
    # replace(is.na(x), NA_real_)
}

#' Data Summary Stats for Pooled Groups
#'
#' Function used by summaryBy or summarise to compute summary stats on pooled data.
#'
#'  Returns N, Min, Max, Lower Quartile, Median, Upper Quartile, Max, Mean, Standard Deviation, Standard Error of the Mean, Coefficient of Variation, and mean confidence intervals.
#' @param x numeric vector
#' @param confInt numeric value giving p value for confidence intervals. If NULL, none are calculated.
#' @param ... parameters to be passed to summary functions such as na.rm or na.omit
#' @export
pooled_sem <- function(x, confInt = 0.05, ...){
  # Remove NAs
  x_ <- stats::na.omit(x)

  # From https://stackoverflow.com/questions/16974389/how-to-calculate-a-pooled-standard-deviation-in-r
  # First use R's vector facilities to define the variables you need for pooling.
  x_["df"] <- x_[grepl("N$", names(x_))] - 1
  x_["s2"] <- x_[grepl("SD$", names(x_))]^2
  x_["ss"] <- x_$s2 * x_$df

  pN <- sum(x_[grepl("N$", names(x_))])
  pMean <- sum(x_[grepl("(?i)mean", names(x_))] * x_[grepl("N$", names(x_))]) / pN

  pSS <- sum(x_$ss)
  pN_ <- prod(x_[grepl("N$", names(x_))][[1]]) / pN
  pMean_ <- sum(x_[grepl("(?i)mean", names(x_))]^2) -
    2 * (x_[1, grepl("(?i)mean", names(x_))] * x_[2, grepl("(?i)mean", names(x_))])

  pSD <- sqrt((pSS + (pN_ * pMean_)) / (pN - 1))

  # Calc sigfigs of SD
  pSF <- signif(pSD, 2)
  if(is.na(pSF)){
    sigFig <- 1
  } else if(pSD - trunc(pSF) == 0){
    sigFig <- 0
  } else{
    sigFig <- nchar(sub('.*\\.', '', pSF - trunc(pSF)))
  }

  # Calculate Five number summary
  fiveNum <- stats::fivenum(x_)

  # Generate summary
  tibble::tibble(
    N = length(x_),
    N.NA = N - length(x),
    Min = fiveNum[1],
    lHinge  = round(fiveNum[2], sigFig),
    Median = fiveNum[3],
    uHinge = round(fiveNum[4], sigFig),
    Max = fiveNum[5],
    SD = round(stats::sd(x_, ...), sigFig),
    Mean = round(mean(x, ...), sigFig),
    SEM = round(SD / sqrt(N), sigFig), # could also be calculated sqrt(var(x)/N)
    CV = round(SD / Mean * 100, 2),
    l95 = round(Mean - stats::qnorm(1 - confInt / 2) * SEM, sigFig),
    u95 = round(Mean + stats::qnorm(1 - confInt / 2) * SEM, sigFig)
  )
}

#' Replace infinite values in data.table
#'
#' Given a data.table with Inf values, replace by reference
#' @param DT data.table
#' @param fillV value to substitute, defaults to NA
#' @export
infReplace <-
  function(DT, fillV = NA_real_) {
    # by number (slightly faster than by name) :
    for (j in seq_len(ncol(DT)))
      set(DT, which(is.infinite(DT[[j]])), j, fillV)
    # Return DT to allow for use in piping
    DT
  }
