#' @title Function \code{scaledown}
#' @description Scale down to a short workflow by 
#' reducing the number of genes in data
#' and iterations of MCMCs
#' @export
scaledown = function(){
  if(!getOption("fbseqStudies.scaledown")) return()
  options("fbseqStudies.backend" = "OpenMP")
  genes = 100
  assign("genes", genes, envir = parent.frame())
  assign("iterations", 100, envir = parent.frame())
  assign("burnin", 10, envir = parent.frame())
  assign("thin", 1, envir = parent.frame())

  data(paschold)
  d = get("paschold")
  d@counts = head(d@counts, genes)
  assign("paschold", d, envir = parent.frame())

  data(tableS3table1)
  d = get("tableS3table1")
  d = head(d, genes)
  assign("tableS3table1", d, envir = parent.frame())
}
