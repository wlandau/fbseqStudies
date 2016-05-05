.onLoad = function(libname, pkgname){
  options(list(
    "fbseqStudies.backend" = "CUDA",
    "fbseqStudies.scaledown" = F
  ))
}
