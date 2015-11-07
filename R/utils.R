#' @title Function \code{pathname}
#' @description name a new directory for storing simulations and results
#' @export
#' @return directory name
pathname = function(){
  s = gsub(" ", "_", Sys.time())
  s = gsub(":", "-", s)
  out = s
  i = 1
  while(out %in% list.files()){
    i = i + 1
    out = paste0(s, "_", i)
  }
  paste0(out, "/")
  out
}

#' @title Function \code{newpath}
#' @description make a new directory for storing simulations and results
#' @export
#' @return directory name
#' @param path path to directory
newpath = function(path = pathname()){
  if(!file.exists(path)) dir.create(path)
  if(substr(path, nchar(path), nchar(path)) != "/") path = paste0(path, "/")
}

#' @title Function \code{my.proc.time}
#' @description \code{proc.time()} as a named numeric vector
#' @export
#' @return a named numeric vector version of \code{proc.time()}
my.proc.time = function(){
  t = proc.time()
  x = as.numeric(t)
  names(x) = names(t)
  x
}

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
