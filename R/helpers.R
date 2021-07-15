BOC_BASE_PATH <- "https://www.bankofcanada.ca/valet"

cache_dir <- function(){
  cache_path <- file.path(tempdir(),"BOC_data_cache")
  if (!dir.exists(cache_path)) dir.create(cache_path)
  cache_path
}

parse_boc_date <- function(date){
  tryCatch(
  new_date<-date %>%
    gsub("Q1$","-02-01",.) %>%
    gsub("Q2$","-05-01",.) %>%
    gsub("Q3$","-08-01",.) %>%
    gsub("Q4$","-11-01",.) %>%
    as.Date(),
  error=function(cond){
    warning("Could not convert date")
    new_date<<-as.Date(NA)
  })
  new_date
}



#' Get series to series group link and date of last available data point
#'
#' @param quiet (Optional) Don't emit messages or warnings if \code{TRUE}, default is \code{FALSE}
#' @return a tibble with series, series group and last available data (if series has dates or \code{NA} if series does not have dates)
#' @export
get_boc_all_series_last_date <- function(quiet=TRUE){
  tmp <- file.path(cache_dir(),"series_group_dates")
  if (!file.exists(tmp)) {
    gs <- list_boc_series_groups()
    d <- get_boc_series_group(gs$name,recent=1,quiet=quiet)
    saveRDS(d,tmp)
  } else {
    d<-readRDS(tmp)
  }
  d
}
