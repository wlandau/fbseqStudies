#' @title Function \code{simulation_duplicated}
#' @description simulation scenario made by simply duplicating the rows and columns of the Paschold data
#' @export
#' @return a list if pertinent scenario information
#' @param genes number of genes
#' @param libraries number of libraries
simulation_duplicated = function(genes = 3.5e4, libraries = 16){
  data(paschold)
  paschold = get("paschold")

  gs = sample.int(nrow(paschold@counts), genes, replace = T)
  ns = 0:(libraries -1) %% ncol(paschold@counts) + 1
  group = (ns + (ns %% 2)) / 2

  design = paschold@design[ns,]
  counts = paschold@counts[gs, ns]

  libnames = colnames(paschold@counts)
  libnames = gsub("B73xMo17_Mo17xB73", "hybrids", libnames)
  libnames = gsub("B73xMo17", "hybrid1", libnames)
  libnames = gsub("Mo17xB73", "hybrid2", libnames)
  libnames = gsub("B73", "parent1", libnames)
  libnames = gsub("Mo17", "parent2", libnames)
  libnames = gsub("_.*", "", libnames)
  libnames = paste0(libnames[ns], "_", 1:libraries)

  rownames(counts) = paste0("gene_", 1:genes)
  colnames(counts) = rownames(design) = libnames

  truth = list(
    gs = gs,
    ns = ns
  )

  supplement = list(
    simulation = "duplicated",
    group = group,
    truth = truth
  )

  scenario = Scenario(
    counts = counts, 
    design = design, 
    contrasts = paschold@contrasts, 
    bounds = paschold@bounds, 
    propositions = paschold@propositions,
    supplement = supplement
  )

  list(scenario = scenario, analyses = list(), simulation = "duplicated")
}
