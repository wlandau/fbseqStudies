#' @include util-analyses.R util-simulations.R util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R
NULL

#' @title Function \code{clean_df}
#' @description clean data frame for plotting
#' @param d raw data frame
#' @export
#' @return clean data frame for plotting
clean_df = function(d){
  d$simulation = relevel_simulations(d$simulation)
  d$analysis = relevel_analyses(d$analysis)
  d$heterosis = relevel_heterosis(d$heterosis)
  d = d[d$simulation %in% simulations() & d$analysis %in% analyses(),]
  d
}
