#' @title Function \code{default_logfiles}
#' @description randomly-generated log file locations to \code{sink()} and delete verbose messages
#' @export
#' @return vector of paths path to log files
default_logfiles = function(){
  n = c("output", "message")
  x = paste(paste(n, "_", sep = ""), gsub(" |:", "_", Sys.time()), sample(1e4:9.9999e4, 1), ".txt", sep = "")
  names(x) = n
  x
}

#' @title Function \code{mysink}
#' @description sink to randomly-generated log file locations
#' @export
#' @return a list of logfiles and the file connection of the message file
mysink = function(){
  logfiles = default_logfiles()
  con = file(logfiles["message"], "w")
  sink(logfiles["output"], type = "output")
  sink(con, type = "message")
  list(files = logfiles, connection = con)
}

#' @title Function \code{unsink}
#' @description unsink message and output. also remove previous logfiles.
#' @export
#' @param logs list of files and the connection for output and messages
unsink = function(logs){
  sink(NULL, type = "output")
  sink(NULL, type = "message")
  close(logs$connection)
  lapply(logs$files, file.remove)
}
