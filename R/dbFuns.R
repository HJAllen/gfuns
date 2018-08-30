##################################################
# Version 3 db wrappers
# database options are set in ~\workspace\.my.cnf
# Group values include:
# remoteharshadb = Harsha Intake
# EPA_harshadb = EPA enterprise harshadb instance
# EPA_chl_wsd_admin = EPA enterprise chl_wsd admin
# EPA_chl_wsd_user = EPA enterprise chl_wsd user
# EPA_chl_wsd_read = EPA enterprise chl_wsd reader
##################################################

#' Wrapper to connect to db
#'
#' Connect to database using RMariaDB. May want to add secure credentials method.
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
dbConnect_ <- function(Group = "EPA_harshadb",
                       defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                       verbose = FALSE, ...){
  # Make Connection
  con <- suppressWarnings(DBI::dbConnect(RMariaDB::MariaDB(),
                                         default.file = defaultFile,
                                         groups = Group))
  # If connection is valid
  if(!DBI::dbIsValid(con)){
    message("Invalid connection")
    stop()
  }else{
    if(verbose) message("Connection Succeeded")
    con
  }
}

#' Function to get data from sql statement
#'
#' Submit sql and get results from database using RMariaDB. May want to add secure credentials method.
#' @param sql sql statement
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
get_sql3 <- function(sql, Group = "EPA_harshadb",
                     defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                     verbose = FALSE, ...){
  on.exit(DBI::dbDisconnect(con))
  tryCatch({
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group)
    # If connection is valid
    if(!DBI::dbIsValid(con)){
      message("Invalid connection")
      stop()
    }else{
      if(verbose)print(con)
      tibble::as_tibble(DBI::dbGetQuery(con, statement = sql))
    }
  }, error = function(e) print(e))
}

#' Function to send sql statement
#'
#' Submit sql statement using RMariaDB. May want to add secure credentials method.
#' @param sql sql statement
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @param CLIENT_MULTI_STATEMENTS_ logical to use CLIENT_MULTI_STATEMENTS
#' @export
send_sql3 <- function(sql, Group = "EPA_harshadb",
                      defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                      verbose = FALSE,
                      CLIENT_MULTI_STATEMENTS_ = TRUE, ...){
  # Set on exit
  on.exit(expr = {
    DBI::dbDisconnect(con)
  })
  # open the connection using user, passsword, etc., as
  tryCatch({
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group
    )
    # If connection is valid continue
    if(DBI::dbIsValid(con)){
      if(verbose) print(con)
      # Send statements
      rs <- DBI::dbSendQuery(con, statement = sql)

      if(DBI::dbHasCompleted(rs)){
        message("Statment succeeded.")
        DBI::dbClearResult(rs)
      }else{
        message("Statement failed.")
        stop()
      }
    }else{
      message("Connection to DB failed")
      stop()
    }

  },error = function(e) print(e))
}

#' Function to append dataframe to db table
#'
#' Submit data to be appended to database table using RMariaDB. Output 0 or 1 based on success if verbose = TRUE May want to add secure credentials method.
#' @param dataF data frame to be appended
#' @param tName table to appand data to
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param header_ logical whether first row is header
#' @param verbose provide feedback to user
#' @export
append_sql3 <- function(dataF, tName,
                        Group = "EPA_harshadb",
                        defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                        header_ = FALSE, verbose = FALSE,
                        ...){
  # Set on exit
  on.exit(DBI::dbDisconnect(con))

  # open the connection using user, passsword, etc., as
  tryCatch({
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group
    )
    # If connection is valid continue
    if(DBI::dbIsValid(con)){
      if(verbose) print(con)
      # Make sure table exists
      if(!DBI::dbExistsTable(con, tName)){
        message(paste(tName, "does not exist"))
        stop()
      }
      # Append the table
      # overwrite = False is default so should fail if record exists
      rs <- DBI::dbWriteTable(conn = con, name = tName,
                              value = dataF, append = TRUE,
                              header = header_, row.names = FALSE)
      if(rs){
        message(paste("Append to ", tName, "succeeded."))
      }else{
        message(paste("Append to ", tName, "failed."))
      }
      # Return logical if verbose
      # otherwise quit silently
      if(verbose) rs
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e) print(e))
}

#' Function to update db table
#'
#' Update database table. Records with extant keys will be updated those without will be inserted using RMariaDB. Output 0 or 1 based on success if verbose = TRUE May want to add secure credentials method.
#' @param dataF data frame to be appended
#' @param tName table to appand data to
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
update_sql3 <- function(dataF, tName, key = NULL,
                        Group = "EPA_harshadb",
                        defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                        verbose = FALSE, ...)
{
  # Set on exit
  on.exit(expr = {
    DBI::dbDisconnect(con)
  })
  # Make sure key is not NULL
  if(is.null(key)){
    message("key cannot be NULL")
    stop()
  }
  # Establish Fields
  fields <- names(dataF)
  # Establish Fields to be updated
  Ufields <- fields[!fields %in% key]

  # Arrange dataF
  dataF_ <-
    dataF %>%
    dplyr::select(dplyr::one_of(Ufields), dplyr::one_of(key)) %>%
    as.list()
  # Remove names for anonymous
  names(dataF_) <- NULL
  # Create UPDATE statement
  sql_ <- paste0("UPDATE ", tName, " set ",
                 paste0(Ufields, "=?", collapse = ", "),
                 " WHERE ",
                 paste(key, " = ?", collapse = " AND "),
                 ";")
  print(sql_)
  # Try UPDATE
  tryCatch({
    # Open the connection
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group)
    # If connection is valid continue
    if(DBI::dbIsValid(con)){
      if(verbose) print(con)
      # Make sure table exists

      if(!DBI::dbExistsTable(con, tName)){
        message(paste(tName, "does not exist"))
        stop()
      }
      # Make sure fields in table
      tFields <- DBI::dbListFields(con, tName)
      if(!all(fields %in% tFields)){
        message(paste("Fields",
                      paste(fields[!fields %in% tFields], collapse = ", "),
                      "not in table"))
      }
      update <- DBI::dbSendStatement(con, sql_)
      DBI::dbBind(update, dataF_)
      message(paste(DBI::dbGetRowsAffected(update), "Rows Updated"))
      DBI::dbClearResult(update)
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e, ...){
    message("Update Failed")
    print(e)
  })
}

#' Function to insert values into db table
#'
#' Uses parameterized statement with option to replace if exists
#'
#' replaceifexists = TRUE is slower
#' Note: REPLACE makes sense only if a table has a PRIMARY KEY or UNIQUE index. Otherwise, it becomes equivalent to INSERT, because there is no index to be used to determine whether a new row duplicates another.
#' @param dataF data frame to be appended
#' @param tName table to appand data to
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
# insert_sql3(dataF = sample.sID, replaceifexists = FALSE, Group = "EPA_harshadb")
insert_sql3 <- function(dataF, tName, replaceifexists = FALSE,
                        Group = "EPA_harshadb",
                        defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                        verbose = FALSE, ...)
{
  # Set on exit
  on.exit(expr = {
    DBI::dbDisconnect(con)
  })

  # Establish Fields
  fields <- names(dataF)

  # Arrange dataF
  dataF_ <-
    dataF %>%
    as.list()
  # Remove names for anonymous
  names(dataF_) <- NULL

  # If updateifexists
  if(replaceifexists){
    # Create REPLACE statement
    sql_ <- paste0("REPLACE INTO ", tName, " (",
                   paste0(fields, collapse = ", "),
                   ") VALUES(",
                   paste(rep("?", length(fields)), collapse = ","),
                   ")")
  }else{
    # Create REPLACE statement
    sql_ <- paste0("INSERT INTO ", tName, " (",
                   paste0(fields, collapse = ", "),
                   ") VALUES(",
                   paste(rep("?", length(fields)), collapse = ","),
                   ")")
  }
  if(verbose){
    print(sql_)
    print(head(dataF))
  }
  # Try UPDATE
  tryCatch({
    # Open the connection
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group)
    # If connection is valid continue
    if(DBI::dbIsValid(con)){
      if(verbose) print(con)
      # Make sure table exists
      tables <- DBI::dbListTables(con)
      if(!tolower(tName) %in% tables){
        message(paste(tName, "does not exist"))
        stop()
      }
      # Make sure fields in table
      tFields <- DBI::dbListFields(con, tName)
      if(!all(fields %in% tFields)){
        message(paste("Fields",
                      paste(fields[!fields %in% tFields], collapse = ", "),
                      "not in table"))
        stop()
      }
      insert <- DBI::dbSendStatement(con, sql_)
      DBI::dbBind(insert, dataF_)
      DBI::dbClearResult(insert)
      message(paste(DBI::dbGetRowsAffected(update), "Rows Affected"))
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e, ...){
    message("Update Failed")
    print(e)
    if(DBI::dbIsValid(con)){
    }
  })
}

#################################################
# End Version 3 db wrappers
##
