#' @title Function \code{simulation_model}
#' @description simulates a scenario using model
#' @export
#' @return a list if pertinent scenario information
#' @param genes number of genes
#' @param libraries number of libraries
simulation_model = function(genes = 3e4, libraries = 16){
  data(paschold)
  paschold = get("paschold")
  s = scenario_heterosis_model(genes, libraries)
  ns = 0:(libraries -1) %% ncol(paschold@counts) + 1
  s@supplement$group = (ns + (ns %% 2)) / 2
  s@supplement$simulation = "model"
  list(scenario = s, analyses = list(), simulation = "model")
}
