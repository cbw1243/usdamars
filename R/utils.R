check_numeric <- function(dat){
  dat_numeric <- suppressWarnings(as.numeric(gsub(',', '', dat)))
  if(sum(dat_numeric, na.rm = T) == 0){
    FALSE
  }else{
    TRUE
  }
}

convert_date <- function(data){
  out <- data
  if('published_date' %in% colnames(out)){
    out[, 'published_date'] <- as.POSIXct(out[, 'published_date'], format = "%m/%d/%Y %H:%M:%S")
  }
  if('report_date' %in% colnames(out)){
    out[, 'report_date'] <- as.Date(out[, 'report_date'], format = "%m/%d/%Y")
  }
  if('report_begin_date' %in% colnames(out)){
    out[, 'report_begin_date'] <- as.Date(out[, 'report_begin_date'], format = "%m/%d/%Y")
  }
  if('report_end_date' %in% colnames(out)){
    out[, 'report_end_date'] <- as.Date(out[, 'report_end_date'], format = "%m/%d/%Y")
  }
  out <- dplyr::mutate_if(out, check_numeric, function(i) as.numeric(gsub(',', '', i)))
  return(out)
}

ams_request_single <- function(slugID = NULL, slugID_legacy = NULL){
  endpoint <- "https://marsapi.ams.usda.gov/services/v1.2/reports/"
  if (!is.null(slugID)){
    response <- httr::GET(paste0(endpoint, '/', slugID), authenticate(key, ""))
  }else{
    response <- httr::GET(paste0(endpoint, '/', slugID_legacy), authenticate(key, ""))
  }
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = 'UTF-8'))
  data_out <- data[['results']]
  return(data_out)
}
