parse_boc_series_json_response <- function(json_data){
  #utils::download.file(paste0(canbank:::BOC_BASE_PATH,paste0("/observations/",s,"/json")),tmp, quiet=TRUE)
  #json_data<-jsonlite::read_json(tmp)
  terms <- json_data$terms
  details <- json_data$seriesDetail
  series <- json_data$observations

  d<-details %>%
    names() %>%
    lapply(function(n){
      dn <- details[[n]]
      dimensions <- dn$dimension
      series %>%
        lapply(function(ss){
          seq(1,length(dimensions$key)) %>%
            lapply(function(ki){
              kn=dimensions$name[ki]
              kv=dimensions$key[ki]
              tibble(!!kn:=ss[[kv]],
                     value=ss[[n]]$v)
            }) %>% bind_rows()
        }) %>%
        bind_rows() %>%
        mutate(name=n,label=dn$label,description=dn$description)
    }) %>%
    bind_rows()
  d
}

parse_boc_series_group_json_response <- function(json_data,sg){
  #utils::download.file(paste0(canbank:::BOC_BASE_PATH,paste0("/observations/group/",sg,"/json")),tmp, quiet=TRUE)
  #json_data<-jsonlite::read_json(tmp)
  terms <- json_data$terms
  group_detail <- json_data$groupDetail
  series_detail <- json_data$seriesDetail
  observations <- json_data$observations

  d <- observations %>%
    lapply(function(o){
      ids <- setdiff(names(o),names(series_detail))
      names(series_detail) %>%
        lapply(function(n){
          tibble(name=n,
                 value=o$v)
        }) %>%
        bind_rows() %>%
        bind_cols(as_tibble(o[ids]))
    }) %>%
    bind_rows() %>%
    mutate(group_name=sg,group_label=group_detail$label,group_descriptions=group_detail$group_descriptions,
           group_link=group_detail$link)

  d<-series_detail %>%
    names() %>%
    lapply(function(n){
      dn <- details[[n]]
      dimensions <- dn$dimension
      series %>%
        lapply(function(ss){
          seq(1,length(dimensions$key)) %>%
            lapply(function(ki){
              kn=dimensions$name[ki]
              kv=dimensions$key[ki]
              tibble(!!kn:=ss[[kv]],
                     value=ss[[n]]$v)
            }) %>% bind_rows()
        }) %>%
        bind_rows() %>%
        mutate(name=n,label=dn$label,description=dn$description)
    }) %>%
    bind_rows()

  d
}

parse_boc_series_group_info_json_response <- function(json_data){
  #utils::download.file(paste0(canbank:::BOC_BASE_PATH,paste0("/groups/",s,"/json")),tmp, quiet=TRUE)
  #json_data<-jsonlite::read_json(tmp)
  terms <- json_data$terms
  details <- json_data$groupDetails
  group_series <- details$groupSeries

  d <- group_series %>%
    names() %>%
    lapply(function(n){
      as_tibble(group_series[[n]]) %>%
        mutate(name=n) %>%
        relocate(.data$name)
    }) %>%
    bind_rows() %>%
    mutate(group_name=details$name,group_label=details$label,group_description=details$description)
}
