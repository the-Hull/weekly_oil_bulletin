## process WOBs

source("./R/downloads.R")

# current wob list -------------------------------------------------------


res <- get_wob_list(
  url = "https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf",
  path_download = 'data/meta/'
)

wobs <- read_wob_list(res$path_file)
wobs <- wobs[!is.na(wobs$url), ]


# initiate dl -------------------------------------------------------------


# load log
logs <- read.csv("./doc/log/log.csv")
logs[['download_date']] <- as.Date(logs[['download_date']])

# non-downloaded data from logs
dl_mask <- make_log_mask(wobs, logs, verbose = TRUE)

# focus on raw data
wobs <- subset(wobs[dl_mask, ], ind %in% c("Data (XLS)"))
# sort by date
wobs <- wobs[order(make_date(wobs$values)), ]

status <- list()

path_data <- "./data/raw"
# for(idx in seq_len(nrow(wobs))){
for(idx in 1:3){


  file_name <- basename(wobs[['url']][idx])

  path_file <- normalizePath(
    file.path(
      path_data,
      file_name
    )
  )

  if(!file.exists(path_file)){
    dl_status <- download.file(
      url = wobs[['url']][idx],
      destfile = path_file,
      mode = 'wb'
    )

    if(dl_status > 0){
      status <- append(status, list(url = wobs[['url']][idx]), status = dl_status)
      message(sprintf("Download for File %s failed, going to next entry", file_name))
      next

    }

  } else {
    message(sprintf("File %s already exists but is not listed in log", file_name))

  }


  if(file.exists(path_file)){


    log_row <- logs[0, ]
    log_row[1 ,c("bulletin", "ind", "values","url")] <-
      wobs[idx, c("bulletin", "ind", "values","url")]

    log_row[1, c("download_date")] <-
      Sys.Date()
    log_row[1, c("in_db")] <-
      c(FALSE)

    logs <- rbind(logs, log_row)

    message(
      sprintf(
        "Added downloaded WOB #%s from %s to download log",
        wobs[['bulletin']][idx],
        file_name))

  }

}

# append logs

# write.table(
#   logs,
#   "./doc/log/log.csv",
#   append = TRUE,
#   row.names = FALSE,
#   col.names = FALSE)



# order wob by date, start at oldest
# grab 1 wob
## download file -> if success, add wob row to log, set download date

## check if file is already in db -> if not: append to db csv





## cycle through wobs, check if exists in log

