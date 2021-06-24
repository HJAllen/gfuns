#' Function to load harshadb tables.
#'
#' Tables from chl_wsd will be loaded/updated. Switch dev will use development version of tables. If the table is not available, look for Rdata file in wd/sysdata.rda
#' @param dbTables character vector of tables to load. If NULL and scheam provided, all tables in schema will be loaded
#' @param schema character giving db schema, used to determine tables to return when dbTables is NULL
#' @param dev boolean to set chl_wsd tables to dev version
#' @param dateLimit Date vector of length to bound select. If NULL, all records returned. If the first value is NA, all records with date < second value returned. If the second value is NA, all records with date > first value returned.
#' @param Group Group identifier found in .my.cnf
#' @param returnClass class of return object. Should be one of data.table, tibble, or data.frame.
#' @param defaultFile file where Group credentials are found
#' @param localFile optional path to local .Rdata or .RDS file to load if DB is unavailable
#' @param forceLocal boolean to force load of localFile, defaults to FALSE
#' @param env environment in which to return data. If null, global is assumed.
#' @param updateSysdata if TRUE, update the table in wd/sysdata.rda, if character, a directory path is assumed
#' @param verbose provide feedback to user
#' @import data.table
#' @export
dbLoad <-
  function(dbTables = NULL,
           schema = NULL,
           dev = FALSE,
           dateLimit = NULL,
           Group = "EPA_harshadb",
           returnClass = "data.table",
           defaultFile = file.path(path$ws, ".my.cnf"),
           localFile = NULL,
           forceLocal = FALSE,
           env = .GlobalEnv,
           updateSysdata = FALSE,
           verbose = FALSE){

    # Testing
    if(!TRUE){
      {
        # dbTables <- "sample_id"
        # schema <- NULL
        dbTables <- NULL
        schema <- c("harshadb", "chl_wsd")[1]

        dev <- FALSE
        dateLimit <- NULL
        Group <- c("EPA_harshadb", "EPA_chl_wsd_user")[1]
        returnClass <- "data.table"
        defaultFile <- file.path(path$ws, ".my.cnf")
        localFile <- file.path(path$ws_chl_wsd, "sysdata.rda")
        forceLocal <- FALSE
        env <- .GlobalEnv
        updateSysdata <- file.path(path$ws_HarshaDB, "sysdata.rda")#!FALSE
        verbose <- !FALSE
      }
      dbTables <-
        c("chl_chainofcustody_log", "chl_sample_log",
          "chl_extraction_log", "chl_extraction",
          "chl_analysis_log", "chl_analysis_template", "chl_analysis",
          "chl_analyst", "chl_project", "chl_reagent_log",
          "chl_instrument", "chl_qa_flags", "chl_sample_type")
      dateLimit <- as.Date(c("2020-01-01", "2020-08-15"))
      dateLimit <- as.Date(c(NA, "2020-08-15"))
      dbTables = NULL

      dbTables <- c("assay", "elisaresults", "elisaresultssummary", "qa_flags", "sample_id")
      dbTables <-
        get_sql3("show tables;") %>%
        .[grepl("_dev", Tables_in_harshadb), Tables_in_harshadb]

      get_sql3("SELECT * FROM information_schema.tables;") %>%
        .[TABLE_SCHEMA == "chl_wsd"]


      dbLoad(dbTables =
               get_sql3("show tables;") %>%
               # .[grepl("_dev", Tables_in_harshadb), Tables_in_harshadb],
               .[grepl("(?i)elisa|sample|qa|assay", Tables_in_harshadb), Tables_in_harshadb],
             # dateLimit = dateLimit,
             env = .GlobalEnv,
             verbose = TRUE)
      dbLoad(dbTables = "sample_id",
             localFile = file.path(path$ws_Nutrients, "sysdata.rda"),
             env = .GlobalEnv,
             verbose = TRUE)
    }

    # Set on.exit
    on.exit({
      if(exists("con")){
        # Close con
        if(is.null(con)){
          rm(con)
        } else if(DBI::dbIsValid(con)){
          DBI::dbDisconnect(con)
          rm(con)
        }
      }
    })

    # Check defaultFile
    if(!file.exists(defaultFile)){
      stop(paste("gfuns::dbLoad: defaultFile:", defaultFile, "Does not exist"))
    }

    # Handle dbTables and schema
    if(is.null(dbTables) & is.null(schema)){
      stop("gfuns::dbLoad: one of dbTables and schema must be !NULL")
    }

    # Handle dateLimit
    if(!is.null(dateLimit) & !inherits(dateLimit, "Date")){
      dateLimit <- as.Date(dateLmit)
    }

    # Test for database availibility
    con <-
      tryCatch({
        if(forceLocal){
          message("gfuns::dbLoad: forcing to localFile")
          NULL
        } else {
          gfuns::dbConnect_()
        }
      },
      error = function(e){
        message("gfuns::dbLoad: unable to connect to DB")
      })

    dbExists <-
      inherits(con, "MariaDBConnection")

    # Close con
    if(is.null(con)){
      rm(con)
    } else if(DBI::dbIsValid(con)){
      DBI::dbDisconnect(con)
      rm(con)
    }

    # If can't connect, look in Robjects
    if(!dbExists){
      if(!forceLocal){
        message(paste("gfuns::loadDB: Can't connect to DB, looking for",
                      paste(dbTables, collapse = ", "), "in",
                      ifelse(!is.null(localFile), localFile, file.path(getwd(), "sysdata.rda"))))
      }

      Tables <-
        data.table::data.table(
          Table = dbTables,
          fromDB = FALSE)

      Tables[, dd := .(.({
        if(is.null(localFile)){
          # look for data directory and prioritize sysdata.rda
          lF <-
            list.dirs(recursive = F) %>%
            {
              if(any(grepl("(?i)data.rda", .))){
                .[grepl("(?i)data.rda$", .)]
              } else if(any(grepl("(?i)^data$", .))){
                .[grepl("(?i)^data$", .)]
              }
            }
        }

        # look for file
        if(length(list.dirs(localFile)) != 0){
          print(ifelse(dev,
                       gsub(".rds", "_dev.rds", Table),
                       paste0(Table,".rds$")))
          lF <-
            list.files(localFile,
                       pattern = ifelse(dev,
                                        gsub(".rds", "_dev.rds", Table),
                                        paste0(Table,".rds$")),
                       full.names = T)
        }
        # print(localFile)
        # print(readRDS(localFile))

        if(length(lF) == 0){
          message(
            paste("gfuns::loadDB: Can't find",
                  file.path(lF)))
        } else if(length(lF) > 1){
          message(
            paste("gfuns::loadDB: Multiple matching files found",
                  file.path(lF)))
        } else if(grepl("(?i)\\.rdata$", lF)){
          load(lF)
          get(Table)
        } else if(grepl("(?i)\\.rds$", lF)){
          readRDS(lF)
        } %>%
          {
            # limit to dateLimit
            if(inherits(., "data.table") & !is.null(dateLimit)){
              # use first date column
              dc <- names(which(unlist(lapply(tmp,
                                              function(x) class(x) == "Date")) == TRUE))
              # Get range and replace with dateLimit where not NA
              dateRange <- range(tmp[[dc]])
              dateRange[!is.na(dateLimit)] <- dateLimit[!is.na(dateLimit)]

              .[.[[dc]] %between% dateRange]

            } else .
          }
        })), by = Table]

      for(i in Tables$Table){
        if(Tables[Table == i, !is.na(dd)]){
          message(
            paste("gfuns::loadDB:",
                  i, "loaded from",
                  file.path(getwd(), "sysdata.rda")))
        } else
          message(
            paste("gfuns::loadDB: Can't find",
                  i, "in",
                  file.path(getwd(), "sysdata.rda")))
      }
    } else {

      # If dbTables is not null but zero length, return NULL
      if(!is.null(dbTables) & length(dbTables) == 0){
        return(NULL)
      } else if(!is.null(dbTables)){
        if(verbose) message(paste("gfuns::dbLoad: Tables",
                                  paste(dbTables, collapse = ", "),
                                  "requested"))
      }

      # get available tables
      tryCatch({
        con <-
          DBI::dbConnect(drv = RMariaDB::MariaDB(),
                         default.file = defaultFile,
                         groups = Group)

        # Get list of available tables in db
        Tables <-
          data.table::data.table(
            Table = DBI::dbListTables(con),
            fromDB = TRUE) %>%
          {
            # If dbTables !NULL, check against available, else all available
            if(!is.null(dbTables)){
              .[Table %in% dbTables]
            } else {
              # get all tables
              .
            }
          }

        Tables[, Fields := .(.({
          sql <- paste0("SELECT * from `", Table, "` limit 1")
          # print(sql)
          result <- DBI::dbSendQuery(con, sql)
          # print(result)
          cI <- DBI::dbColumnInfo(result)
          # print(cI)
          DBI::dbClearResult(result)
          data.table::setDT(cI)
        })), by = Table]

        # Disconnect con
        if(DBI::dbIsValid(con)){
          DBI::dbDisconnect(con)
        }
      },
      # Error function will attempt to load data saved as Robject
      error = function(e){
        message(paste("gfuns::dbLoad:",
                      e))#"dbConnect failed reading available tables.")
      })

      # If dev, change dbTables by appending _dev
      if(dev) Tables[, Tables := paste0(Tables, "_dev")]

      # Process in error handling
      # Returns named list containing loaded tables
      Tables[, dd := .(.({
        tryCatch({
          if(verbose) message(paste("gfuns::dbLoad: fetching", Table))

          if(any(grepl("(?i)posix", .SD[, Fields[[1]][, type]])) &
             !is.null(dateLimit)){
            dL <- c(paste0(.SD[, Fields[[1]][grepl("(?i)posix", type), name]],
                           " >= '", dateLimit[1], "'"),
                    paste0(.SD[, Fields[[1]][grepl("(?i)posix", type), name]],
                           " <= '", dateLimit[2], "'"))

            sql <-
              ifelse(any(is.na(dateLimit)),
                     paste0("Select * from `", Table, "` where",
                           dL[!is.na(dateLimit)]),
                     paste0("Select * from `", Table, "` where",
                           dL[1], "and ", dL[2]))

          } else {
            sql <- paste0("Select * from `", Table, "`;")
          }

          get_sql3(sql,
                   Group = Group,
                   returnClass = returnClass,
                   defaultFile = defaultFile,
                   verbose = FALSE) %>%
            # drop leading _ from column names
            data.table::setnames(gsub("^_", "", names(.))) %>%
            # Convert Date and Time classes to IDate and ITime
            .[, lapply(.SD, function(x){
              if(inherits(x, "Date")){
                data.table::as.IDate(x)
              } else if(inherits(x, "hms")){
                data.table::as.ITime(x)
              } else x
            })]
        },

        # Error function
        error = function(e){
          message(paste("gfuns::dbLoad: failed fecthing from db,", e))})
      })), by = Table]
    }

    # if updateSysdata, write to wd/sysdata.rda
    runUpdate <-
      if(is.logical(updateSysdata)){
        updateSysdata
      } else TRUE

    if(runUpdate & Tables[, any(fromDB)]){
      # set filePath
      Tables[, filePath := {
          filePath <-
            if(is.logical(updateSysdata)){
              file.path(getwd(), "sysdata.rda", paste0(Table, ".rds"))
            } else if(dir.exists(updateSysdata)){
              file.path(updateSysdata, paste0(Table, ".rds"))
            } else warning("sysdata.rda not found")
      }, by = Table]

      # Update
      Tables[, updated := {
        tryCatch({
          # Check filePath
          if(!dir.exists(dirname(filePath))){
            dir.create(dirname(filePath))
          }

          if(verbose) message(
            paste("gfuns::dbLoad: updating", filePath))

          lastWriteTime <-
            if(file.exists(filePath)){
              file.info(file = filePath)$mtime
            } else NA

          saveRDS(data.table::copy(.SD)$dd[[1]], file = filePath)

          resultWriteTime <-
            file.info(file = filePath)$mtime

          if(is.na(lastWriteTime) | resultWriteTime > lastWriteTime & verbose){
            message(paste("gfuns::dbLoad: Updated", filePath))
            TRUE
          } else FALSE
        }, error = function(e) {
          message(paste("gfuns::dbLoad:", e))
          FALSE})
      }, by = Table, .SDcols = "dd"]
    } else Tables[, updated := FALSE]

    # Handle env
    if(!is.null(env)){
      # Assign tables to env
      Tables[, loaded := {
        tryCatch({
          if(verbose) message(paste("gfuns::dbLoad: loading", Table,
                                    "to", environmentName(env)))
          result <-
            inherits(assign(gsub("^_", "", Table),
                            data.table::copy(.SD$dd[[1]]),
                            envir = env),
                     "data.frame")
          if(result & verbose){
            message(paste("gfuns::dbLoad: loaded", Table, "to", environmentName(env)))
          }
          result
        }, error = function(e) {
          message(paste("gfuns::dbLoad:", e))
          FALSE})
      }, by = Table, .SDcols = "dd"]
      # return info
      Tables[, .(Table, loaded, updated)]
    } else {
      # if env is not global, return list
      return(dbTables)
    }

  }


