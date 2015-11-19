#' @title Function \code{dir_name}
#' @description name a new directory for storing simulations and results
#' @export
#' @return directory name
dir_name = function(){
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

#' @title Function \code{newdir}
#' @description make a new directory for storing simulations and results
#' @export
#' @return directory name
#' @param path path to directory
newdir = function(path = dir_name()){
  if(!file.exists(path)) dir.create(path)
  if(substr(path, nchar(path), nchar(path)) != "/") path = paste0(path, "/")
  path
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

#' @title Function \code{meta}
#' @description get metadata from a file name
#' @export
#' @return metadata from a file name
#' @param s file name
meta = function(s){
  s = gsub(".rds", "", s)
  p = strsplit(s, "_")[[1]]
  names(p) = c("simulation", "genes", "libraries", "rep", "analysis")[1:length(p)]
  p
}

#' @title Function \code{over_sims}
#' @description apply a function over simulation lists and save to another directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply to each simulation list
over_sims = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    out = fun(l)
    saveRDS(out, paste0(to, f))
  }
}

#' @title Function \code{over_ans}
#' @description apply a function over every \code{analyses} element of every simulation list in a directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply to each \code{analyses} element of every simulation list. Must take in 
#' the simulation list and analysis list, in that order.
over_ans = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    for(a in l$analyses){
      print(paste0("  ", a$analysis))
      m = c(meta(f), a$analysis)
      f2 = paste0(to, paste(m, collapse = "_"), ".rds")
      if(file.exists(f2)) next
      out = fun(l, a)
      saveRDS(out, f2)
    }
  }
}

#' @title Function \code{over_out}
#' @description apply a function over every analysis-specific output file in a given directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
#' @param fun function to apply over every analysis-specific output file in a given directory
over_out = function(from, to, fun){
  from = newdir(from)
  to = newdir(to)
  fs = list.files(from)
  fs = fs[grep(".rds", fs)]
  for(f in fs){
    print(f)
    l = readRDS(paste0(from, f))
    if(file.exists(paste0(to, f))) next
    out = fun(l)
    saveRDS(out, paste0(to, f))
  }
}
