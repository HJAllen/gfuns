#' Dilution calculations
#'
#' Quick calculation of concentration and volume of a dilution. Note it does no checking to make sure units are consistent. Three of four parameters must be supplied
#' @param x data frame with columns v = c("c1", "v1", "c2", "v2"), value = value for variables with NA for variable to solve, units = units of values
#' @export
dilCalc <-
  function(x){
    # Make sure 3 of 4 are given
    if(sum(!is.na(x$value)) != 3){
      stop("At least 3 parameters must be supplied")
    }

    # Check vector names
    if(any(!x$v %in% c("c1", "v1", "c2", "v2"))){
      stop("check your vector names")
    }

    # Change mu to u
    x$units <- gsub(u2c(0xb5), "u", x$units)

    # Copy of x
    x0 <- x

    # Convert concentration to g/L
    for(v in x$v[grepl("c", x$v) & !is.na(x$value)]){
      x$value[x$v == v] <-
        conv_multiunit_(x$value[x$v == v],
                        tolower(x$units[x$v == v]), "g/l")
    }
    x$units[grepl("c", x$v)] <- "g/l"

    # Convert volumes to L
    for(v in x$v[grepl("v", x$v) & !is.na(x$value)]){
      x$value[x$v == v] <-
        measurements::conv_unit(x$value[x$v == v],
                                tolower(x$units[x$v == v]),
                                "l")
    }
    x$units[grepl("v", x$v)] <- "l"

    # Based on which is.null, do calculation c1 * v1 = c2 * v2
    x_ <- x$v[is.na(x$value)]
    x$value[x$v == x_] <-
      switch(x_,
             "c1" = (x$value[x$v == "c2"] * x$value[x$v == "v2"]) / x$value[x$v == "v1"],
             "v1" = (x$value[x$v == "c2"] * x$value[x$v == "v2"]) / x$value[x$v == "c1"],
             "c2" = (x$value[x$v == "c1"] * x$value[x$v == "v1"]) / x$value[x$v == "v2"],
             "v2" = (x$value[x$v == "c1"] * x$value[x$v == "v1"]) / x$value[x$v == "c2"],
             stop("Nothing to solve for, check your vector names")
      )

    # Convert concentration to original
    for(v in x$v[grepl("c", x$v)]){
      x$value[x$v == v] <-
        conv_multiunit_(x$value[x$v == v],
                        tolower(x$units[x$v == v]),
                        tolower(x0$units[x0$v == v]))
      x$units[x$v == v] <- tolower(x0$units[x0$v == v])
    }

    # Convert volumes to L
    for(v in x$v[grepl("v", x$v)]){
      x$value[x$v == v] <-
        measurements::conv_unit(x$value[x$v == v],
                                tolower(x$units[x$v == v]),
                                tolower(x0$units[x0$v == v]))
      x$units[x$v == v] <- tolower(x0$units[x0$v == v])
    }

    # Change u to mu
    x$units <- gsub("u", u2c(0xb5), x$units)

    # Create out column
    x$out <- paste(x$value, x$units)

    # Return x
    x

  }

# Old Version
# dilCalc <-
#   function(x = c(c1 = NULL, v1 = NULL, c2 = NULL, v2 = NULL),
#            cUnitsIn = "mg/L",
#            cUnitsOut = "mg/L",
#            vUnitsIn = "L",
#            vUnitsOut = "L"){
#     # drop NA
#     x <- x[!is.na(x)]
#     # Make sure 3 of 4 are given
#     if(length(x) != 3){
#       stop("At least 3 parameters must be supplied")
#     }
#
#     # Check vector names
#     x_ <- c("c1", "v1", "c2", "v2")
#     if(any(!names(x) %in% x_)){
#       stop("check your vector names")
#     }
#
#     # Based on which is.null, do calculation
#     x_ <- x_[!x_ %in% names(x)]
#     x[x_] <-
#       switch(x_,
#              "c1" = (x["c2"] * x["v2"]) / x["v1"],
#              "v1" = (x["c2"] * x["v2"]) / x["c1"],
#              "c2" = (x["c1"] * x["v1"]) / x["v2"],
#              "v2" = (x["c1"] * x["v1"]) / x["v1"],
#              stop("Nothing to solve for, check your vector names")
#       )
#
#     # Convert volume units
#     if(x_ %in% c("v1", "v2") & vUnitsIn != vUnitsOut){
#       x[x_] <-
#         measurements::conv_unit(x[x_], tolower(vUnitsIn), tolower(vUnitsOut))
#     }
#
#     # Convert concentration units
#     if(x_ %in% c("c1", "c2") & cUnitsIn != cUnitsOut){
#       x[x_] <-
#         conv_multiunit_(x[x_], tolower(cUnitsIn), tolower(cUnitsOut))
#     }
#
#     # Change u to mu
#     cUnitsOut <- gsub("u", u2c(0xb5), cUnitsOut)
#     vUnitsOut <- gsub("u", u2c(0xb5), vUnitsOut)
#
#     out <- tibble::tibble(x[x_],
#                           ifelse(grepl("v", x_),
#                                  vUnitsOut,
#                                  cUnitsOut)
#     )
#
#     names(out) <- c(x_,
#                     ifelse(grepl("v", x_),
#                            "vUnits",
#                            "cUnits")
#     )
#     out
#
#   }

#' Modified conv_multiunit from measurements package
#'
#' Allows from and to units to have no whitespace between units
#' @param x numeric vector of measurement value
#' @param from initial units
#' @param to output units
#' @export
conv_multiunit_ <-
  function (x = 1, from, to) {
    ops_f = gsub(pattern = "[^/\\*]", replacement = "", from)
    ops_t = gsub(pattern = "[^/\\*]", replacement = "", to)
    if (ops_f != ops_t)
      stop("The order the units in \"from\" and \"to\" must be equivalent.")
    ops = c(unlist(strsplit(ops_f, split = "")), "")
    froms = unlist(strsplit(from, split = "/|\\*"))
    froms_bad = froms[!froms %in% unlist(measurements::conv_unit_options)]
    if (length(froms_bad) > 0)
      stop(paste(froms_bad, "is not supported in the \"from\" argument. See conv_unit_options for supported units."))
    tos = unlist(strsplit(to, split = "/|\\*"))
    tos_bad = tos[!tos %in% unlist(measurements::conv_unit_options)]
    if (length(tos_bad) > 0)
      stop(paste(tos_bad, "is not supported in the \"to\" argument. See conv_unit_options for supported units."))
    convs = sapply(1:length(froms), function(i) {
      measurements::conv_unit(1, froms[i], tos[i])
    })
    conv_val = paste(c(rbind(convs, ops)), collapse = " ")
    conv_val = eval(parse(text = conv_val))
    return(x * conv_val)
  }
