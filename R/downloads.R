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

# helpers -----------------------------------------------------------------



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

make_log_mask <- function(wobs, logs, verbose = FALSE){

  `%nin%` <- Negate(`%in%`)



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

# procedure ---------------------------------------------------------------

# check if ./log/log.csv

## if TRUE: subset wobs_list to only include undownloaded urls
## achieve by looking at newest date only?
## achieve by complete comparison?

## if FALSE: use whole wobs_list

# download files - keep track of success/failure, files in folders

# update log



# test --------------------------------------------------------------------

# res <- get_wob_list(
#   url = "https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf",
#   path_download = 'data/meta/'
#   )
#
# wobs <- read_wob_list(res$path_file)
# log <- wobs[0, ]
# log$download_date <- character(0)
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
