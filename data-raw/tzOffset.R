Olson <- data.frame(OlsonNames = OlsonNames(), offset = NA, stringsAsFactors = F)
for(i in 1:nrow(Olson)){
  # print(Olson$OlsonNames[i])
  Olson$offset[i] <- tryCatch(
    (as.numeric(as.POSIXct("1970-01-01", tz = "UTC")) - as.numeric(as.POSIXct("1970-01-01", tz = Olson$OlsonNames[i]))),
    error = function(e)NA_integer_
  )
}
devtools::use_data(Olson)



