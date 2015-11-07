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
