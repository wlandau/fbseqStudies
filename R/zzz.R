#' @title .onLoad
#' @description fbseqStudies .onLoad
#' @export
#' @param libname libname
#' @param pkgname pkgname
.onLoad = function(libname, pkgname){
  options(list(
    "fbseqStudies.scaledown" = F,
    "fbseqStudies.scaledown.genes" = 30,
    "fbseqStudies.scaledown.iterations" = 100,
    "fbseqStudies.scaledown.burnin" = 100,
    "fbseqStudies.scaledown.thin" = 1
  ))
}
