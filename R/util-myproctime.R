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
