#' @title Function \code{simulations}
#' @description analysis to show in plots
#' @export
#' @return analysis simulations
simulations = function(){
  out = c("Simple", "Model", "edgeR")
  ordered(out, levels = out)
}
