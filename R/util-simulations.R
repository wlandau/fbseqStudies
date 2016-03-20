#' @title Function \code{simulations}
#' @description analysis to show in plots
#' @export
#' @return analysis simulations
simulations = function(){
  out = c("Simple", "Model", "edgeR")
  ordered(out, levels = out)
}

#' @title Function \code{case_simulations}
#' @description analysis to show in plots
#' @export
#' @return analysis simulations
case_simulations = function(){
  out = c("Simple", "Model", "edgeR")
  ordered(out, levels = out)
}

#' @title Function \code{priors_simulations}
#' @description analysis to show in plots
#' @export
#' @return analysis simulations
priors_simulations = function(){
  out = c("normal", "t", "Laplace")
  ordered(out, levels = out)
}
