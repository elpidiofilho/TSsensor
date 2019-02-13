#' import data from raw data
#'
#' import raw file data in Campbell .dat format
#'
#' @param filename name of file to be imported extension
#' @param  RetOpt type of information to be imported. "info" import data heads and "data" import all data
#' @return dataframe
#' @export
importCSdata = function(filename, RetOpt = "data"){
  if (RetOpt %in% c('data', 'info')) {
    if(RetOpt == "info"){
      stn.info = scan(file = filename, nlines = 4, what = character(), sep = "\r")
      return(stn.info)
    } else {
      header = scan(file = filename, skip = 1, nlines = 1 ,what = character(), sep = ",")
      stn.data = utils::read.table(file = filename, skip = 4, header = FALSE, na.strings = c("NAN"),sep=",")
      names(stn.data) = header
      stn.data$TIMESTAMP = as.POSIXct(strptime(stn.data$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="GMT"))
      return(stn.data)}
  } else {

    print('invalid RetOpt argument, allowed values are "data" or "info"')
    return(NULL)
  }
}

#' show gaps in times series
#'
#' @importFrom dplyr left_join mutate filter %>%
#' @param d dataframe with times series
#' @return dataframe
#' @export
showgap <- function(d) {
  data.length <- length(d$TIMESTAMP)
  time.min <- d$TIMESTAMP[1]
  time.max <- d$TIMESTAMP[data.length]
  all.dates <- data.frame(TIMESTAMP =  seq(time.min, time.max, by="hour"))
  dgap <- left_join(all.dates,d) %>% mutate(rsum = rowSums(is.na(.))) %>%
    filter(rsum > 0)
  return(dgap)
}

#' fill gaps in times series with mean values
#'
#' @importFrom dplyr left_join mutate_if %>%
#' @param d dataframe with times series
#' @return dataframe
#' @export
fillgap <- function(d) {
  data.length <- length(d$TIMESTAMP)
  time.min <- d$TIMESTAMP[1]
  time.max <- d$TIMESTAMP[data.length]
  all.dates <-data.frame(TIMESTAMP =  seq(time.min, time.max, by="hour"))
  dgap = left_join(all.dates,d) %>% mutate(rsum = rowSums(is.na(.))) %>%
    mutate_if(is.numeric, funs(na.mean(.)))
  return(dgap)
}

#' calculate daily mean
#'
#' @importFrom dplyr mutate select_if group_by summarise_all
#' @importFrom lubridate year month day
#' @param d dataframe with times series
#' @return dataframe
#' @export
daily_mean <- function(d) {
  d1 = d %>% mutate(ano = year(TIMESTAMP), mes = month(TIMESTAMP), dia = day(TIMESTAMP))
  d.dia = d1 %>% select_if(is.numeric) %>% group_by(ano, mes, dia) %>%
    summarise_all(.funs = mean)
  return(d.dia)
}

