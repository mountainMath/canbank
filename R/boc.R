
#' List available BOC time series
#'
#' The list of series is cached for the duration of the current R session
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#' @return a tibble with series information
#'
#' @examples
#' \donttest{
#' series <- list_boc_series()
#' }
#' @export
list_boc_series <- function(refresh=FALSE){
  tmp <- file.path(cache_dir(),"series_list")
  if (refresh || !file.exists(tmp)) {
    message("Downloading BOC series list...")
    utils::download.file(paste0(BOC_BASE_PATH,"/lists/series/csv"),tmp, quiet=TRUE)
  } else {
    message("Accessing BOC series list from cache...")
  }
  r<-readr::read_lines(tmp)
  start <- which(r=="SERIES")
  d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c"))
  d
}

#' List available BOC time series groups
#'
#' The list of series is cached for the duration of the current R session
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#' @return a tibble with series information
#'
#' @examples
#' \donttest{
#' series_groups <- list_boc_series_groups()
#' }
#' @export
list_boc_series_groups <- function(refresh=FALSE){
  tmp <- file.path(cache_dir(),"series_groups_list")
  if (refresh || !file.exists(tmp)) {
    message("Downloading BOC series list...")
    utils::download.file(paste0(BOC_BASE_PATH,"/lists/groups/csv"),tmp, quiet=TRUE)
  } else {
    message("Accessing BOC series list from cache...")
  }
  r<-readr::read_lines(tmp)
  start <- which(r=="GROUPS")
  d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c"))
  d
}


#' Get information on BOC time series
#'
#' The result is cached for the duration of the current R session
#' @param series A vector of series identifiers
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#' @return a tibble with series metadata
#'
#' @examples
#' \donttest{
#' series_info <- get_boc_series_info(c("IEXE1001.CL", "IEXE4702"))
#' }
#' @export
get_boc_series_info <- function(series,refresh=FALSE){
  series %>%
    lapply(function(s){
      tmp <- file.path(cache_dir(),paste0("series_info_",s))
      if (refresh || !file.exists(tmp)) {
        message(paste0("Downloading BOC series data for ",s))
        utils::download.file(paste0(BOC_BASE_PATH,paste0("/series/",s,"/csv")),tmp, quiet=TRUE)
      } else {
        message("Accessing BOC series list from cache...")
      }
      r<-readr::read_lines(tmp)
      start <- which(r=="SERIES DETAILS")
      d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c"))
      d
    }) %>%
  bind_rows()
}


#' Get information on BOC time series groups
#'
#' The result is cached for the duration of the current R session
#' @param series_group A vector of series group identifiers
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#' @return a tibble with series metadata
#'
#' @examples
#' \donttest{
#' series_group_info <- get_boc_series_group_info(c("FX_RATES_ANNUAL", "FSR-2017-JUNE-CHART-3"))
#' }
#' @export
get_boc_series_group_info <- function(series_group,refresh=FALSE){
  series_group %>%
    lapply(function(s){
      tmp <- file.path(cache_dir(),paste0("series_group_info_",s))
      if (refresh || !file.exists(tmp)) {
        message(paste0("Downloading BOC series data for ",s))
        utils::download.file(paste0(BOC_BASE_PATH,paste0("/groups/",s,"/csv")),tmp, quiet=TRUE)
      } else {
        message("Accessing BOC series list from cache...")
      }

      r<-readr::read_lines(tmp)

      start <- which(r=="GROUP DETAILS")
      blank <- which(r=="")
      stop <- which(r=="GROUP SERIES")
      if (length(blank>1)) stop <- pmin(stop,blank[2])
      d2<-readr::read_csv(tmp,skip=start,
                          n_max = stop-start-2,
                          col_types = readr::cols(.default="c"))

      r<-readr::read_lines(tmp)
      start <- which(r=="GROUP SERIES")
      d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c")) %>%
        mutate(group_name=d2$name,group_label=d2$label,group_description=d2$description)
      d
    }) %>%
    bind_rows()
}



#' Get BOC time series data
#'
#' The data is cached for the duration of the current R session
#' @param series A vector of series identifiers
#' @param start_date (Optional) Start date for time series
#' @param end_date (Optional) End date for time series
#' @param recent (Optional) Only retrieve most recent \code{recent} number of data points
#' @param recent_weeks (Optional) Only retrieve data for most recent \code{recent_weeks} weeks
#' @param recent_months (Optional) Only retrieve data for most recent \code{recent_months} months
#' @param recent_years (Optional) Only retrieve data for most recent \code{recent_years} years
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#'
#' @return a tibble with series information
#'
#' @examples
#' \donttest{
#' series_data <- get_boc_series(c("IEXE1001.CL", "IEXE4702"))
#' }
#' @export
get_boc_series <- function(series,
                           start_date=NULL,end_date=NULL,
                           recent=NULL,recent_weeks=NULL,recent_months=NULL,recent_years=NULL,
                           refresh=FALSE){

  query = c()
  if (!is.null(start_date)||!is.null(end_date)) {
    if (!is.null(recent)||!is.null(recent_weeks)||!is.null(recent_months)||!is.null(recent_years))
      warning("Can't specify recent time frame if also specifying start or end dates, ignoring rencent time frame specifications.")
    if (!is.null(start_date)) query <- c(query,start_date=as.character(start_date))
    if (!is.null(end_date)) query <- c(query,end_date=as.character(end_date))
  } else {
    if (!is.null(recent)) query <- c(query,recent=as.character(recent))
    if (!is.null(recent_weeks)) query <- c(query,recent_weeks=as.character(recent_weeks))
    if (!is.null(recent_months)) query <- c(query,recent_months=as.character(recent_months))
    if (!is.null(recent_years)) query <- c(query,recent_years=as.character(recent_years))
    if (length(query)>1) {
      warning(paste0("Can only specify one recent time frame, using ",names(query)[1]))
      query<-query[1]
    }
  }

  series %>%
    lapply(function(s){
      hash <- digest::digest(c("series_data",query))
      tmp <- file.path(cache_dir(),paste0("series_data_",s,"_",hash))
      if (refresh || !file.exists(tmp)) {
        message(paste0("Downloading BOC series data for ",s))
        url <- paste0("/observations/",s,"/csv")

        if (length(query)>0) {
          query_string <- names(query) %>%
            lapply(function(n)paste0(n,"=",query[[n]]))  %>%
            paste0(collapse="&")
          url <- paste0(url,"?",query_string)
        }
        utils::download.file(paste0(BOC_BASE_PATH,url),tmp, quiet=TRUE)
      } else {
        message("Accessing BOC series list from cache...")
      }
      r<-readr::read_lines(tmp)
      start <- which(r=='"SERIES"')
      blank <- which(r=="")
      stop <- which(r=='"OBSERVATIONS"')
      if (length(blank>1)) stop <- pmin(stop,blank[2])
      d2<-readr::read_csv(tmp,skip=start,
                          n_max = stop-start-2,
                          col_types = readr::cols(.default="c"))

      start <- which(r=='"OBSERVATIONS"')
      d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c")) %>%
        mutate(date=parse_boc_date(.data$date)) %>%
        tidyr::pivot_longer(-.data$date,names_to="series",values_to="value") %>%
        mutate(value=as.numeric(.data$value)) %>%
        left_join(d2,by=c("series"="id"))
      d
    }) %>%
    bind_rows()
}


#' Get BOC time series data for a series group
#'
#' The data is cached for the duration of the current R session
#' @param series_group A vector of series group identifiers
#' @param start_date (Optional) Start date for time series
#' @param end_date (Optional) End date for time series
#' @param recent (Optional) Only retrieve most recent \code{recent} number of data points
#' @param recent_weeks (Optional) Only retrieve data for most recent \code{recent_weeks} weeks
#' @param recent_months (Optional) Only retrieve data for most recent \code{recent_months} months
#' @param recent_years (Optional) Only retrieve data for most recent \code{recent_years} years
#' @param refresh (Optional) Refresh currenly cached series if \code{TRUE}, default is \code{FALSE}
#'
#' @return a tibble with series information
#'
#' @examples
#' \donttest{
#' series_group_data <- get_boc_series_group(c("FX_RATES_ANNUAL", "FSR-2017-JUNE-CHART-3"))
#' }
#' @export
get_boc_series_group <- function(series_group,
                           start_date=NULL,end_date=NULL,
                           recent=NULL,recent_weeks=NULL,recent_months=NULL,recent_years=NULL,
                           refresh=FALSE){

  query = c()
  if (!is.null(start_date)||!is.null(end_date)) {
    if (!is.null(recent)||!is.null(recent_weeks)||!is.null(recent_months)||!is.null(recent_years))
      warning("Can't specify recent time frame if also specifying start or end dates, ignoring rencent time frame specifications.")
    if (!is.null(start_date)) query <- c(query,start_date=as.character(start_date))
    if (!is.null(end_date)) query <- c(query,end_date=as.character(end_date))
  } else {
    if (!is.null(recent)) query <- c(query,recent=as.character(recent))
    if (!is.null(recent_weeks)) query <- c(query,recent_weeks=as.character(recent_weeks))
    if (!is.null(recent_months)) query <- c(query,recent_months=as.character(recent_months))
    if (!is.null(recent_years)) query <- c(query,recent_years=as.character(recent_years))
    if (length(query)>1) {
      warning(paste0("Can only specify one recent time frame, using ",names(query)[1]))
      query<-query[1]
    }
  }

  series_group %>%
    lapply(function(s){
      hash <- digest::digest(c("series_group_data",query))
      tmp <- file.path(cache_dir(),paste0("series_data_",s,"_",hash))
      if (refresh || !file.exists(tmp)) {
        message(paste0("Downloading BOC series data for ",s))
        url <- paste0("/observations/group/",s,"/csv")

        if (length(query)>0) {
          query_string <- names(query) %>%
            lapply(function(n)paste0(n,"=",query[[n]]))  %>%
            paste0(collapse="&")
          url <- paste0(url,"?",query_string)
        }
        utils::download.file(paste0(BOC_BASE_PATH,url),tmp, quiet=TRUE)
      } else {
        message("Accessing BOC series list from cache...")
      }
      r<-readr::read_lines(tmp)
      start <- which(r=='"SERIES"')
      blank <- which(r=="")
      stop <- which(r=='"OBSERVATIONS"') -1
      #if (length(blank>1)) stop <- pmin(stop,blank[2])
      d2<-readr::read_csv(tmp,skip=start,
                          n_max = stop-start-2,
                          col_types = readr::cols(.default="c"))

      start <- which(r=='"OBSERVATIONS"')
      d<-readr::read_csv(tmp,skip=start,col_types = readr::cols(.default="c")) %>%
        mutate(date=parse_boc_date(.data$date)) %>%
        tidyr::pivot_longer(-.data$date,names_to="series",values_to="value") %>%
        mutate(value=as.numeric(.data$value)) %>%
        left_join(d2,by=c("series"="id"))
      d %>%
        mutate(group_name=s)
    }) %>%
    bind_rows()
}

#' @import dplyr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


