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

#' @title Function \code{case_clean_df}
#' @description clean data frame for plotting
#' @param d raw data frame
#' @export
#' @return clean data frame for plotting
case_clean_df = function(d){
  d$simulation = relevel_simulations(d$simulation)
  d$analysis = relevel_analyses(d$analysis)
  d$heterosis = relevel_heterosis(d$heterosis)
  d = d[d$simulation %in% case_simulations() & d$analysis %in% case_analyses(),]
  d
}

#' @title Function \code{priors_clean_df}
#' @description clean data frame for plotting
#' @param d raw data frame
#' @export
#' @return clean data frame for plotting
priors_clean_df = function(d){
  d$simulation = relevel_simulations(d$simulation)
  d$analysis = priors_relevel_analyses(d$analysis)
  d$heterosis = relevel_heterosis(d$heterosis)
  d = d[d$simulation %in% simulations() & d$analysis %in% priors_analyses(),]
  d
}
