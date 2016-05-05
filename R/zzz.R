#' @title .onLoad
#' @description fbseqStudies .onLoad
#' @export
#' @param libname libname
#' @param pkgname pkgname
.onLoad = function(libname, pkgname){
  options(list(
    "fbseqStudies.scaledown" = F
  ))
}

#' @title .onUnload
#' @description set fbseqStudies options
#' @export
#' @param libname libname
#' @param pkgname pkgname
.onUnload = function(libname, pkgname){
  options(list(
    "fbseqStudies.scaledown" = NULL
  ))
}
