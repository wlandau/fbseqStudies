#' @include util-mytheme.R
NULL

#' @title Function \code{comparehprobs}
#' @description plot information about coverage of beta parameters in credible intervals
#' @export
#' @param from directory containing mcmc results on real data
#' @param from_s3 directory containing classification information of genes according to Paschold (2012)
#' @param to directory to save plots
comparehprobs = function(from, from_s3, to){

# from = "~/home/work/projects/thesis_data/mcmc/real_mcmc/"
# from_s3 = "~/home/work/projects/thesis_data/analyze/real_analyze/tableS3/"
# to = "~/home/work/projects/thesis_data/analyze/real_analyze/comparehprobs/"

  from = newdir(from)
  from = newdir(from_s3)
  to = newdir(to)

  if(!file.exists(paste0(from_s3, "tableS3part.txt"))) return()
  groups = read.table(paste0(from_s3, "tableS3part.txt"), sep = "\t", header = T, stringsAsFactors = F)

  l = readRDS(paste0(from, list.files(from)[1]))
  a = l$analyses[["fullybayes+normal"]]
  p = as.data.frame(probs(a$chains)[,c("high-parent_B73xMo17", "low-parent_B73xMo17", "high-parent_Mo17xB73", "low-parent_Mo17xB73")])
  colnames(p) = c("hph_bm", "lph_bm", "hph_mb", "lph_mb")
  p$gene = rownames(p)
  d = melt(p, id.vars = "gene")

  g2 = data.frame(
    hph_bm = groups$BxM %in% 5:6,
    lph_bm = groups$BxM %in% 7:8,
    hph_mb = groups$MxB %in% 5:6,
    lph_mb = groups$MxB %in% 7:8,
    gene = rownames(p)
  )
  d2 = melt(g2, id.vars = "gene")

  colnames(d) = c("gene", "heterosis", "probability")
  d$paschold = ifelse(d2$value, "discovery", "nondiscovery") 
  levels(d$heterosis) = c(
    "hph_bm" = "high-parent BxM", 
    "lph_bm" = "low-parent BxM", 
    "hph_mb" = "high-parent MxB", 
    "lph_mb" = "low-parent MxB"
  )

  pl = ggplot(d) + 
    geom_histogram(aes_string(x = "probability")) + 
    facet_grid(paschold~heterosis, scales = "free_y") + 
    theme_few() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))

  ggsave(paste0(to, "comparehprobs.pdf"), pl, width = 8, height = 4, dpi = 1200)
  ggsave(paste0(to, "comparehprobs.ps"), pl, width = 8, height = 4, dpi = 1200)
  ggsave(paste0(to, "comparehprobs.eps"), pl, width = 8, height = 4, dpi = 1200)
  ggsave(paste0(to, "comparehprobs.tiff"), pl, width = 8, height = 4, dpi = 1200)

  data(paschold)
  paschold = get("paschold")
  ct = paschold@counts

  for(type in levels(d$heterosis)){
    x = d[d$heterosis == type,]
    no = x[x$paschold == "nondiscovery",]
    no = no[order(no$probability, decreasing = T),]
    nogenes = no$gene[1:10]
    noct = ct[nogenes,]
    nom = as.data.frame(t(apply(noct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
    nom = cbind(rownames(nom), no$probability[1:10], nom)
    colnames(nom) = c("gene", "probability", unique(gsub("_[0-9]", "", colnames(noct))))
    rownames(nom) = NULL
    str = print(xtable(nom), include.rownames=F, sanitize.text.function=function(x){x}, 
      hline.after = 0)
    write(str, file = paste0(to, gsub(" ", "-", type), "_high-prob-nondiscoveries.tex"))

    x = d[d$heterosis == type,]
    no = x[x$paschold == "nondiscovery",]
    no = no[order(no$probability, decreasing = F),]
    nogenes = no$gene[1:10]
    noct = ct[nogenes,]
    nom = as.data.frame(t(apply(noct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
    nom = cbind(rownames(nom), no$probability[1:10], nom)
    colnames(nom) = c("gene", "probability", unique(gsub("_[0-9]", "", colnames(noct))))
    rownames(nom) = NULL
    str = print(xtable(nom), include.rownames=F, sanitize.text.function=function(x){x}, 
      hline.after = 0)
    write(str, file = paste0(to, gsub(" ", "-", type), "_low-prob-nondiscoveries.tex"))

    x = d[d$heterosis == type,]
    yes = x[x$paschold == "discovery",]
    yes = yes[order(yes$probability, decreasing = F),]
    yesgenes = yes$gene[1:10]
    yesct = ct[yesgenes,]
    yesm = as.data.frame(t(apply(yesct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
    yesm = cbind(rownames(yesm), yes$probability[1:10], yesm)
    colnames(yesm) = c("gene", "probability", unique(gsub("_[0-9]", "", colnames(yesct))))
    rownames(yesm) = NULL
    str = print(xtable(yesm), include.rownames=F, sanitize.text.function=function(x){x}, 
      hline.after = 0)
    write(str, file = paste0(to, gsub(" ", "-", type), "_low-prob-discoveries.tex"))

    x = d[d$heterosis == type,]
    yes = x[x$paschold == "discovery",]
    yes = yes[order(yes$probability, decreasing = T),]
    yesgenes = yes$gene[1:10]
    yesct = ct[yesgenes,]
    yesm = as.data.frame(t(apply(yesct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
    yesm = cbind(rownames(yesm), yes$probability[1:10], yesm)
    colnames(yesm) = c("gene", "probability", unique(gsub("_[0-9]", "", colnames(yesct))))
    rownames(yesm) = NULL
    str = print(xtable(yesm), include.rownames=F, sanitize.text.function=function(x){x}, 
      hline.after = 0)
    write(str, file = paste0(to, gsub(" ", "-", type), "_high-prob-discoveries.tex"))
  }
}
