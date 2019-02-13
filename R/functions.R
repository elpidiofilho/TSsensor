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
#' @importFrom imputeTS na.mean
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


#' calculate monthly mean
#'
#' @importFrom dplyr mutate select_if group_by summarise_all
#' @importFrom lubridate year month day
#' @param d dataframe with times series
#' @return dataframe
#' @export
montly_mean <- function(d) {
  d1 = d %>% mutate(ano = year(TIMESTAMP), mes = month(TIMESTAMP))
  d.mes = d1 %>% select_if(is.numeric) %>% group_by(ano, mes) %>% summarise_all(.funs = mean)
  return(d.mes)
}

#' calculate yearly mean
#'
#' @importFrom dplyr mutate select_if group_by summarise_all
#' @importFrom lubridate year month day
#' @param d dataframe with times series
#' @return dataframe
#' @export
yearly_mean <- function(d) {
  d1 = d %>% mutate(ano = year(TIMESTAMP))
  d.ano = d1 %>% select_if(is.numeric) %>% group_by(ano) %>% summarise_all(.funs = mean)
  return(d.ano)
}

#' calculate thawing, freezing, isotermal and freezethaw days
#'
#' @importFrom dplyr mutate select group_by summarise_all summarise_at ungroup
#' @importFrom lubridate year month day as_datetime
#' @param timestam vector with date values
#' @param vt vector with time series
#' @return list
#' @export
tfif_days <- function(timestamp, vt) { ## achar um nome melhor para esta função

  d = data.frame(timestamp, temperatura = vt) %>%
    mutate(timestamp = lubridate::as_datetime(timestamp)) %>%
    mutate(ano = year(timestamp), mes = month(timestamp), dia = day(timestamp))

  dg = d %>% mutate(temp_posit = ifelse(temperatura > 0,1, 0 )) %>%
    mutate(temp_negativ = ifelse(temperatura < 0, 1, 0 ))  %>%
    mutate(temp_maior_05 =  ifelse(temperatura > 0.5, 1, 0 )) %>%
    mutate(temp_menor_05 =  ifelse(temperatura < -0.5, 1, 0 )) %>%
    mutate(temp_isoterm =  ifelse(temperatura >= -0.5, ifelse(temperatura <= 0.5, 1, 0 ), 0))

  dsum_day = dg %>% group_by(ano,mes,dia) %>%
    summarise_at(c('temp_posit', 'temp_negativ', 'temp_maior_05',
                   'temp_menor_05', 'temp_isoterm'),  sum) %>% ungroup %>%
    mutate(thawing = ifelse(temp_posit == 24 & temp_maior_05 >= 1 , 1, 0)) %>%
    mutate(freezing = ifelse(temp_negativ == 24 & temp_menor_05 >= 1 , 1, 0)) %>%
    mutate(isotermal = ifelse(temp_isoterm == 24  , 1, 0)) %>%
    mutate(freezethaw = ifelse(temp_maior_05 >= 1  & temp_menor_05 >= 1 , 1, 0))

  dsum_day = dsum_day %>% group_by(ano, mes, dia) %>% summarise_all(funs(tot= sum)) %>%
    select(ano,mes,dia,thawing_tot:freezethaw_tot) %>% ungroup()

  dsum_month = dsum_day %>% select(-dia) %>% group_by(ano, mes) %>% summarise_all(funs(month = sum))

  dsum_year = dsum_day %>% select(-dia, -mes) %>% group_by(ano) %>% summarise_all(funs(year = sum))
  return(list(tot_day = dsum_day, tot_month = dsum_month, tot_year = dsum_year))
}
