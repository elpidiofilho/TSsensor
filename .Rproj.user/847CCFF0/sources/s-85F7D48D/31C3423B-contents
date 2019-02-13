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



