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
