#' @title Function \code{ci_beta}
#' @description Info to assesses coverage of betaparameters by credible intervals.
#' @export
#' @return Info to assesses coverage of betaparameters by credible intervals.
#' @param from directory with simulation _levels
#' @param to where to put ci information
ci_beta = function(from, to){
  ci_beta_level(from, to, level = 0.5)
  ci_beta_level(from, to, level = 0.95)
}