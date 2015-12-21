#' @title Function \code{ci_level}
#' @description Info to assesses coverage of hyperparameters by credible intervals.
#' @export
#' @return Info to assesses coverage of hyperparameters by credible intervals.
#' @param from directory with simulation lists
#' @param to where to put ci information
ci_hyper = function(from, to){
  ci_hyper_level(from, to, level = 0.5)
}