% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbFuns.R
\name{update_sql3}
\alias{update_sql3}
\title{Function to update db table}
\usage{
update_sql3(
  dataF,
  tName,
  key = NULL,
  Group = "EPA_harshadb",
  defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
  verbose = FALSE,
  ...
)
}
\arguments{
\item{dataF}{data frame to be appended}

\item{tName}{table to appand data to}

\item{Group}{Group identifier found in .my.cnf}

\item{defaultFile}{file where Group credentials are found}

\item{verbose}{provide feedback to user}
}
\description{
Update database table. Records with extant keys will be updated those without will not be inserted using RMariaDB. Output 0 or 1 based on success if verbose = TRUE May want to add secure credentials method.
}
