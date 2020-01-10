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
                       defaultFile = file.path(path$ws, ".my.cnf"),
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
#' @param returnClass class of return object. Should be one of data.table, tibble, or data.frame.
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
get_sql3 <- function(sql, Group = "EPA_harshadb",
                     returnClass = "data.table",
                     defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                     verbose = FALSE, ...){
  # Set on exit
  on.exit(expr = {
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
  })

  tryCatch({
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group)
    # If connection is valid
    if(!DBI::dbIsValid(con)){
      message("Invalid connection")
      stop()
    }else{
      if(verbose) print(con)
      DBI::dbGetQuery(con, statement = sql) %>%
        switch(returnClass,
               data.table = data.table::as.data.table(.),
               tibble = tibble::as_tibble(.),
               .)
    }
  }, error = function(e) print(e))
}

#' Function to send sql statement
#'
#' Submit sql statement using RMariaDB. May want to add secure credentials method.
#' @param dataF If given, data frame of key values to be used in parameterized statement.
#' @param tName table name
#' @param returnClass class of return object. Should be one of data.table, tibble, or data.frame.
#' @param sql sql statement. defaults to select
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @param CLIENT_MULTI_STATEMENTS_ logical to use CLIENT_MULTI_STATEMENTS
#' @export
send_sql3 <- function(dataF,
                      tName,
                      returnClass = "data.table",
                      sql = "SELECT * FROM ",
                      Group = "EPA_harshadb",
                      defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                      verbose = FALSE,
                      CLIENT_MULTI_STATEMENTS_ = TRUE, ...){

  # dataF <- gfuns::get_sql3("SELECT  * from sample_id;") %>%
  #   tail(10)

  # Set on exit
  on.exit(expr = {
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
    if(exists("rs")){
      suppressWarnings(DBI::dbClearResult(rs))
      rm(rs)
    }
  })

  # If sql is a data set, run parameterized query, else run sql as query
  if(!inherits(dataF, "data.frame")){
    stop("dataF is not a data.frame")
  }

  # Establish Fields
  fields <- names(dataF)

  # Get primary key(s)
  PK <- get_PrimaryKeys(tName, Group = Group)

  # Make sure sql has all primary keys
  if(!all(PK %in% fields)){
    stop(paste(PK[!PK %in% fields], "While attempting parameterized sql, not all Primary Keys in table"))
  }

  # Arrange dataF
  dataF_ <-
    dataF %>%
    dplyr::select(dplyr::one_of(PK)) %>%
    # Arrange dataF
    as.list() %>%
    # Remove names for anonymous
    unname()

  # Create Select statement
  sql <- paste0(sql, tName, " WHERE ",
                paste0("`", PK, "`", collapse = "= ? AND "), " = ?;")

  if(verbose) print(dim(dataF))
  if(verbose) print(head(dataF))
  if(verbose) print(sql_)

  # open the connection using user, passsword, etc., as
  tryCatch({
    con <- gfuns::dbConnect_(defaultFile = defaultFile,
                             Group = Group)
    # If connection is valid continue
    if(DBI::dbIsValid(con)){
      if(verbose) print(con)
      # Send statements
      rs <- DBI::dbSendQuery(con, statement = sql)
      # If parameterized statement
      if(!is.null(dataF)){
        DBI::dbBind(rs, dataF_)
      }
      result <-
        DBI::dbFetch(rs) %>%
        switch(returnClass,
               data.table = data.table::as.data.table(.),
               tibble = tibble::as_tibble(.),
               .)

      if(DBI::dbHasCompleted(rs)){
        if(verbose) message("Statment succeeded.")
        DBI::dbClearResult(rs)
        return(result)
      }else{
        message("Statement failed.")
        return(NULL)
      }
    }else{
      message("Connection to DB failed")
      return(NULL)
    }

  },error = function(e) print(e))
}
#
# dataF_ <-
#   dataF[, c("Sample_ID", "CoC_ID")] %>%
#   as.list()
# names(dataF_) <- NULL
# rs <- DBI::dbSendQuery(con, paste("SELECT * FROM", tName, "WHERE Sample_ID = ? And CoC_ID = ? AND isnull(QA_CoC"))
# DBI::dbBind(rs, dataF_)
# DBI::dbFetch(rs)
# DBI::dbClearResult(rs)




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
  on.exit(expr = {
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
    if(exists("rs")){
      rm(rs)
    }
  })

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
      # Return logical if verbose otherwise quit silently
      if(verbose){
        message(paste("Append to ", tName, "succeeded =", rs))
        rs
      }
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e) print(e))
}

#' Function to update db table
#'
#' Update database table. Records with extant keys will be updated those without will not be inserted using RMariaDB. Output 0 or 1 based on success if verbose = TRUE May want to add secure credentials method.
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
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
    if(exists("rs")){
      suppressWarnings(DBI::dbClearResult(rs))
      rm(rs)
    }
  })

  # Make sure key is not NULL
  if(is.null(key)){
    message("key cannot be NULL")
    stop()
  }

  # Check if key is a primary key
  PK <- get_PrimaryKeys(tName, Group = Group)
  if(!all(key %in% PK)){
    message(paste(key[!key %in% PK], "Is not a Primary Key"))
    return(NULL)
  }

  # Establish Fields
  fields <- names(dataF)
  # Establish Fields to be updated
  Ufields <- fields[!fields %in% key]

  # Arrange dataF
  dataF <-
    dataF %>%
    dplyr::select(dplyr::one_of(Ufields), dplyr::one_of(key))# %>%
    # cbind(., .[,names(.) %in% Ufields])

  # Arrange dataF
  dataF_ <-
    dataF %>%
    as.list() %>%
    # Remove names for anonymous
    unname()

  # Create UPDATE statement
  sql_ <- paste0("UPDATE ", tName, " set ",
                 paste0("`", Ufields, "`=?", collapse = ", "),
                 " WHERE ",
                 paste0("`", key, "`=?", collapse = " AND "),
                 ";")

  if(verbose) print(dim(dataF))
  if(verbose) print(head(dataF))
  if(verbose) print(sql_)

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

      # DBI::dbExecute(con, sql_, param = dataF_)
      rs <- DBI::dbSendStatement(con, sql_)
      DBI::dbBind(rs, dataF_)
      rowsAffected <- DBI::dbGetRowsAffected(rs)
      if(verbose) message(paste(rowsAffected, "Rows Updated"))
      DBI::dbClearResult(rs)
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e, ...){
    message("Update Failed")
    print(e)
  })

  # Return rowsAffected
  rowsAffected
}

#' Function to insert or update on duplicate key
#'
#' Insert data using parameterized statement. If duplicate key exists perform Update. Records with extant keys will be updated those without will be inserted using RMariaDB.
#'
#'  Return value is number of rows affected
#' @param dataF data frame to be appended
#' @param tName table to appand data to
#' @param Group Group identifier found in .my.cnf
#' @param defaultFile file where Group credentials are found
#' @param verbose provide feedback to user
#' @export
upsert_sql3 <- function(dataF, tName, key = NULL,
                        Group = "EPA_harshadb",
                        defaultFile = file.path(gfuns::sg("ws"), ".my.cnf"),
                        verbose = FALSE, ...)
{
  # Set on exit
  on.exit(expr = {
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
    if(exists("rs")){
      suppressWarnings(DBI::dbClearResult(rs))
      rm(rs)
    }
  })

  # Make sure key is not NULL
  if(is.null(key)){
    message("key cannot be NULL")
    return(NULL)
  }

  # Check if key is a primary key
  PK <- get_PrimaryKeys(tName, Group = Group)
  if(!(all(key %in% PK) | all(key %in% PK))){
    if(!all(key %in% PK)){
    message(paste(key[!key %in% PK], "Is not a Primary Key"))
    } else if(!all(key %in% PK)){
      message(paste(PK[!PK %in% key], "Primary Key is missing"))
    }
    return(NULL)
  }

  # Establish Fields
  fields <- names(dataF)
  # Establish Fields to be updated
  Ufields <- fields[!fields %in% key]

  # Arrange dataF
  dataF <-
    dataF %>%
    dplyr::select(dplyr::one_of(Ufields), dplyr::one_of(key)) #%>%
    # cbind(., .[,names(.) %in% Ufields])

  # Arrange dataF
  dataF_ <-
    dataF %>%
    as.list() %>%
    # Remove names for anonymous
    unname()

  # Create Insert statement
  sql_ <- paste("INSERT INTO", tName, "(",
                paste0("`", c(Ufields, key), "`", collapse = ", "), ")",
                paste0("Values(", paste0(rep("?", length(c(Ufields, key))), collapse = ", "), ")"),
                "ON DUPLICATE KEY UPDATE",
                paste0("`", Ufields, "`=VALUES(?)", collapse = ", "),
                ";")

  if(verbose) print(dim(dataF))
  if(verbose) print(head(dataF))
  if(verbose) print(sql_)

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

      # DBI::dbExecute(con, sql_, param = dataF_)
      rs <- DBI::dbSendStatement(con, sql_)
      DBI::dbBind(rs, dataF_)
      rowsAffected <- DBI::dbGetRowsAffected(rs)
      if(verbose) message(paste(rowsAffected, "Rows Updated"))
      DBI::dbClearResult(rs)
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e, ...){
    message("Update Failed")
    print(e)
  })

  # Return value
  rowsAffected
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
  # Test
  if(!TRUE){
    tName <- "chl_sample_log_dev"
    replaceifexists <- FALSE
    Group <- "EPA_chl_wsd_admin"
    defaultFile <- file.path(gfuns::sg("ws"), ".my.cnf")
    verbose <- T
    CoCdir <- normalizePath("L:\\Priv\\Cin\\NRMRL\\Chlorophyll\\CoC")
    files <- matrix(list.files(CoCdir, pattern = ".+xlsm$", full.names = TRUE),
                    ncol = 1, dimnames = list(NULL, "files")) %>%
      data.table::as.data.table() %>%
      .[!grepl("2016|Date", files)]
    dataF <- readxl::read_excel(files$files[1], sheet = 1) %>%
      data.table::as.data.table() %>%
      .[, Type := ifelse(grepl("sample", Type), "Sample", Type)] %>%
      .[, c("Sample_ID", "Project", "Collection_Date", "CoC_ID")]
  }

  # Set on exit
  on.exit(expr = {
    # Clear result set
    if(exists("rs")){
      suppressWarnings(DBI::dbClearResult(rs))
      rm(rs)
    }
    # Disconnect con
    if(exists("con")){
      suppressWarnings(DBI::dbDisconnect(con))
      rm(con)
    }
  })

  # Establish Fields
  fields <- names(dataF)

  # Arrange dataF
  dataF_ <-
    dataF %>%
    as.list() %>%
    # Remove names for anonymous
    unname()

  # Create sql statement
  # If replaceifexists use replace otherwise select
  if(replaceifexists){
    # Create REPLACE statement
    sql_ <- paste0("REPLACE INTO ", tName, " (",
                   paste0("`", fields, "`", collapse = ", "),
                   ") VALUES(",
                   paste(rep("?", length(fields)), collapse = ","),
                   ")")
  }else{
    # Create SELECT statement
    sql_ <- paste0("INSERT INTO ", tName, " (",
                   paste0("`", fields, "`", collapse = ", "),
                   ") VALUES(",
                   paste(rep("?", length(fields)), collapse = ","),
                   ")")
  }

  # Feedback
  if(verbose){
    print(sql_)
    print(utils::head(dataF))
  }

  # Try INSERT
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
        stop()
      }
      rs <- DBI::dbSendStatement(con, sql_)
      DBI::dbBind(rs, dataF_)
      rowsAffected <- DBI::dbGetRowsAffected(rs)
      if(verbose) message(paste(rowsAffected, "Rows Affected"))
      DBI::dbClearResult(rs)
    }else{
      message("Connection to DB failed")
      stop()
    }
  }, error = function(e, ...){
    message("Insert Failed")
    print(e)
    if(DBI::dbIsValid(con)){
    }
  })
  rowsAffected
}

#' Get primary keys from MySQL table
#'
#' Function will request table information from MySQL database information_schema.table_constraints table.
#' @param tName table name for which requesting primary keys
#' @param Group Group name linked to my.cnf with connection information
#' @return Returns a one column (COLUMN_NAME) data.table with primary key fields.
#' @export
get_PrimaryKeys <- function(tName, Group = "EPA_harshadb"){
  get_sql3(paste0('SELECT k.COLUMN_NAME
  FROM information_schema.table_constraints t
  LEFT JOIN information_schema.key_column_usage k
  USING(constraint_name,table_schema,table_name)
  WHERE t.constraint_type="PRIMARY KEY"
  AND t.table_schema=DATABASE()
           AND t.table_name="', tName, '";'),
           Group = Group)[["COLUMN_NAME"]]

  # Alternative statement:
  # SELECT COLUMN_NAME
  # FROM information_schema.KEY_COLUMN_USAGE
  # WHERE TABLE_NAME = 'sample_id'
  # AND CONSTRAINT_NAME = 'PRIMARY';
}

# get_sql3("SELECT @@global.time_zone, @@session.time_zone")
# get_sql3("select from_unixtime(86400-3600)")
# get_sql3("select timediff(now(),convert_tz(now(),@@session.time_zone,'+00:00'));")
# get_sql3("select time_format(timediff(now(),convert_tz(now(),@@session.time_zone,'+00:00')),'%H%i');")
# get_sql3("SELECT @@system_time_zone;")
# get_sql3("SELECT TIMEDIFF(NOW(), UTC_TIMESTAMP);")

# tzTest <-
#   data.table::data.table(datetime = Sys.time()) %>%
#   .[, datetimeChar := format(datetime, "%F %X %z")]
# tzTest <-
#   data.table::rbindlist(
#     list(tzTest,
#          data.table::copy(tzTest)[, `:=` (
#            datetime = lubridate::force_tz(datetime, tzone = "EST"),
#            datetimeChar = format(lubridate::force_tz(datetime, tzone = "EST"), "%F %X %z"))]),
#     use.names = T, fill = T)
#
# gfuns::insert_sql3(tzTest, "tzTest")
#
# tzTest_ <-
#   get_sql3("select * from tzTest;")
#
# str(tzTest)
# lubridate::tz(tzTest_$datetime[1])
# lubridate::force_tz(tzTest_$datetime, tzone = "EST")
#
# sample_id <- get_sql3("Select SampleDateTime, TimeZone, SampleDateTimeChar from sample_id")
# str(sample_id)
# sample_id
# lubridate::tz(sample_id[, .(SampleDateTime)])
# lubridate::force_tz(sample_id[, .(SampleDateTime)], tzone = "EST")
# lubridate::with_tz(sample_id[, .(SampleDateTime)], tzone = "Etc/GMT+5")
# lubridate::with_tz(sample_id[, .(SampleDateTime)], tzone = "Etc/GMT-5")
# as.POSIXct(sample_id[, SampleDateTimeChar]) %>% head()
#
# sample_id[, SampleDateTime_ := as.POSIXct(paste0(SampleDateTimeChar, "00"), format = "%F %X %z", tz = "Etc/GMT+5")]
