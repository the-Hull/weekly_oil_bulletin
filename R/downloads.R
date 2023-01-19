get_wob_list <- function(url, path_download = tempdir()){

  path_file <- normalizePath(
    file.path(
      path_download,
      "wob_list.pdf")
  )

  dl_status <- download.file(
    url,
    destfile = path_file,
    mode = 'wb'
  )

  return(
    list(
      path_file = path_file,
      dl_status = dl_status))

}

download_wobs <- function(wobs, logs,   path_data = "./data/raw"){



  # non-downloaded data from logs
  dl_mask <- make_log_mask(wobs, logs, verbose = TRUE)

  # focus on raw data
  wobs <- subset(wobs[dl_mask, ], ind %in% c("Data (XLS)"))
  # sort by date
  wobs <- wobs[order(make_date(wobs$values)), ]

  fails <- list()

  for(idx in seq_len(nrow(wobs))){


    file_name <- basename(wobs[['url']][idx])

    path_file <- file.path(
      path_data,
      file_name
    )

    if(!file.exists(path_file)){
      dl_status <- download.file(
        url = wobs[['url']][idx],
        destfile = path_file,
        mode = 'wb'
      )

      if(dl_status > 0){
        fails <- append(fails, list(url = wobs[['url']][idx]), status = dl_status)
        message(sprintf("Download for File %s failed, going to next entry\n", file_name))
        next

      }

    } else if(file.exists(path_file) & wobs[['url']][idx] %in% logs[['url']]){
      message(sprintf("File %s already exists and is listed in log\n", file_name))
      next

    }


    if(file.exists(path_file) & wobs[['url']][idx] %nin% logs[['url']]){


      log_row <- logs[0, ]
      log_row[1 ,c("bulletin", "ind", "values","url")] <-
        wobs[idx, c("bulletin", "ind", "values","url")]

      log_row[1, c("download_date")] <-
        as.character(Sys.Date())
      log_row[1, c("in_db")] <-
        c(FALSE)
      log_row[1, c("path_file")] <- path_file

      logs <- rbind(logs, log_row)

      message(
        sprintf(
          "Added downloaded WOB #%s from %s to download log\n --- \n",
          wobs[['bulletin']][idx],
          file_name))

    }

  }

  return(logs)

}


# get WOB meta and links
read_wob_list <- function(path_file){

  wob_list <- pdftools::pdf_text(path_file)

  rows<-scan(
    textConnection(wob_list),
    what="character",
    sep = "\n")

  category <- gsub("^\\s+", "", strsplit(rows[2], "   ")[[1]], perl = TRUE)
  category <- category[nchar(category)>0]

  type <- strsplit(rows[3], "[ ]+", perl = TRUE)[[1]]
  wob_colnames <- paste(category, type)

  data_meta <- strsplit(rows[4:length(rows)], "\\s+")
  n_columns <- length(data_meta[[1]])
  column_list <- vector('list', n_columns)

  for(j in seq_along(column_list)){
    for(i in seq_along(data_meta)){

      column_list[[j]][i] <- data_meta[[i]][j]
    }
  }

  wob_list <- as.data.frame(do.call(cbind, column_list))
  colnames(wob_list) <- wob_colnames


  wob_list <- cbind(bulletin = wob_list[,1], (stack(wob_list[, -1])[c(2,1)]))
  wob_list$ind <- as.character(wob_list$ind)

  wob_list$url <- make_wob_url(
    bulletin = wob_list$bulletin,
    type = wob_list$ind,
    date = wob_list$values)


  return(wob_list)


}



make_db <- function(path_dir_db, logs){

  path_db_bin <- file.path(path_dir_db, "wob_full.rds")
  path_db_csv <- file.path(path_dir_db, "wob_full.csv")


  stopifnot('No entries in logs yet.' = nrow(logs) > 0)

  # grab path file and in db NA/FALSE

  db_mask <- (is.na(logs[['in_db']]) | !logs[['in_db']]) &
    (!is.na(logs[['path_file']]) | nchar(logs[['path_file']]) > 0)

  wobs_to_db <- logs[db_mask, ]

  db <- vector('list', nrow(wobs_to_db))

  coltypes <- c(
    "date",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text"
  )

  for(idx in seq_len(nrow(wobs_to_db))){

    # check if file exists, if not, clear log position
    if(!file.exists(wobs_to_db[idx, 'path_file'])){

      wobs_to_db[idx, 'path_file'] <- NA

    } else {

      db[[idx]] <- readxl::read_excel(
        path = wobs_to_db[idx, 'path_file'],
        col_types = coltypes,
        col_names = TRUE,
        na = c("N.A", "N/A"))[-c(122,123), ]

      # remove , and force numeric for the last three columns
      db[[idx]][,7:9] <- apply(
        db[[idx]][,7:9],
        MARGIN = 2,
        FUN = function(x) as.numeric(gsub("[,]", "", x))
      )



      wobs_to_db[idx, 'in_db'] <- TRUE

    }


  }

  db <- do.call(rbind, db)

  if(!file.exists(path_db_csv)){

    # delim
    write.table(
      db,
      path_db_csv,
      col.names = TRUE,
      row.names = FALSE,
      sep = ";")



  } else if(file.exists(path_db_csv)){
    write.table(
      db,
      path_db_csv,
      col.names = FALSE,
      row.names = FALSE,
      sep = ";",
      append = TRUE)

  }

  if(!file.exists(path_db_bin)){

    # binary
    saveRDS(db, path_db_bin)
  } else if(file.exists(path_db_bin)){

    db <- rbind(readRDS(path_db_bin), db)
    saveRDS(db, path_db_bin)

  }

  logs[db_mask, ] <- wobs_to_db

  return(logs)


}


init_logs <- function(path_log){

  if(!file.exists(path_log)){
    log <- structure(list(bulletin = character(0), ind = character(0), values = character(0),
                          url = character(0), download_date = character(0), path_file = character(0),
                          in_db = logical(0)), row.names = integer(0), class = "data.frame")
    write.table(
      log,
      path_log,
      row.names = FALSE,
      col.names = TRUE,
      sep = ",")
    message(sprintf("Initiated empty log in %s\n", path_log))

  } else {
    message(sprintf("Log already exists in %s. Loading\n", path_log))
    log <- read.csv(path_log)
    # log[['download_date']] <- as.Date(log[['download_date']])

  }
  return(log)
}


update_logs <- function(logs, path_log){

  if(!file.exists(path_log)){
    write.table(
      logs,
      path_log,
      col.names = TRUE,
      row.names = FALSE,
      sep = ",")
  } else {
    write.table(
      logs,
      path_log,
      col.names = FALSE,
      row.names = FALSE,
      sep = ",",
      append = TRUE)
  }

}


# helpers -----------------------------------------------------------------



make_wob_url <- function(bulletin, type, date){

  missing_mask <- is.na(date)

  n_identical_obs <- length(unique(lengths(list(bulletin, type, date))))

  if(n_identical_obs > 1){
    stop("All arguments must have the same lengths")
  }

  base_url <- "http://ec.europa.eu/energy/observatory/reports/"





  data_format <- pdf_or_xlsx(type)
  type <- tolower(gsub("[ ]+[(].*$", "", type))
  type <- gsub("\\s", "_", type)

  type <- ifelse(type == 'data', 'raw_data', type)

  date <- as.Date(strptime(date, format = '%d/%m/%Y'))
  date_switch_to_xlsx <- date>=as.Date("2018-09-10")

  date <- format(date, '%Y_%m_%d')

  # system switches to xlsx
  data_format[date_switch_to_xlsx & data_format=='xls'] <- 'xlsx'



  urls <- paste0(
    base_url,
    date,
    "_",
    type,
    "_",
    bulletin,
    ".",
    data_format)


  urls[missing_mask] <- NA

  return(urls)

}


pdf_or_xlsx <- function(str){

  sapply(
    str,
    function(x){

      if(grepl("xlsx?", x, ignore.case = TRUE, perl = TRUE)){
        ftype <- "xls"
      } else {
        ftype <- "pdf"
      }
      return(ftype)
    },
    USE.NAMES = FALSE
  )

}

`%nin%` <- Negate(`%in%`)
make_log_mask <- function(wobs, logs, verbose = FALSE){




  if(nrow(logs) == 0){

    mask <- !logical(nrow(wobs))

  } else {

    mask <- wobs[['url']] %nin% logs[['url']]

  }

  if(verbose){
    message(sprintf("Found %d wob's for download\n", length(mask)))
  }

  return(mask)

}


make_date <- function(x){
  return(strptime(x, "%d/%m/%Y"))
}

# check_db <- function(path_db)


# test --------------------------------------------------------------------
#
# res <- get_wob_list(
#   url = "https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf",
#   path_download = 'data/meta/'
# )
#
# wobs <- read_wob_list(res$path_file)
# log <- wobs[0, ]
# log$download_date <- character(0)
# log$path_file <- character(0)
# log$in_db <- logical(0)
# write.table(log, "./doc/log/log.csv", row.names = FALSE, col.names = TRUE, sep = ",")
#
# make_log_mask(wobs, "./doc/log/log.csv", verbose = TRUE)
#
#
#
# update_log <- function(path_dir_data, path_file_db, path_log){
#
#
#
#
#
# }
#
#
