## process WOBs

source("./R/downloads.R")

path_log <- "./doc/log/log.csv"
url_wob_list <- "https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf"
path_dir_db <- "./data/db/"

# current wob list -------------------------------------------------------


res <- get_wob_list(
  url = url_wob_list,
  path_download = 'data/meta/'
)

wobs <- read_wob_list(res$path_file)
wobs <- wobs[!is.na(wobs$url), ]


logs <- init_logs(path_log)

# download wobs -------------------------------------------------------------



logs <- download_wobs(wobs = wobs, logs = logs,   path_data = "./data/raw")


# write db  -------------------------------------------------------------
logs <- make_db(path_dir_db, logs)


# db checks!
# update logs -------------------------------------------------------------
update_logs(logs = logs, path_log = path_log)



