#' @title Function \code{simulation_paschold}
#' @description plugs the real Paschold dataset into the simulation workflow
#' @export
#' @return a list if pertinent scenario information
simulation_paschold = function(){
  data(paschold)
  paschold = get("paschold")
  scaledown()
  ns = 1:ncol(paschold@counts)
  paschold@supplement$group = (ns + (ns %% 2)) / 2
  paschold@supplement$simulation = "paschold"  
  list(scenario = paschold, analyses = list(), simulation = "paschold")
}
