BOC_BASE_PATH <- "https://www.bankofcanada.ca/valet"

cache_dir <- function(){
  cache_path <- file.path(tempdir(),"BOC_data_cache")
  if (!dir.exists(cache_path)) dir.create(cache_path)
  cache_path
}

parse_boc_date <- function(date){
  date %>%
    gsub("Q1$","-02-01",.) %>%
    gsub("Q2$","-05-01",.) %>%
    gsub("Q3$","-08-01",.) %>%
    gsub("Q4$","-11-01",.) %>%
    as.Date()
}
