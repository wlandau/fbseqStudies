#' @include util-myrelevel.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{paper_case_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_case_figures = function(){

# library(fbseqStudies); library(reshape2); library(plyr); library(ggthemes); setwd("~/home/work/projects/thesis_data/results")

# control parms
dir = newdir("case_study_paper_figures")
extns = c("pdf", "ps", "eps")

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:10)

# fig:hypercoverage
dir_hypercoverage = newdir(paste0(dir, "fig_hypercoverage"))
level = 0.5
l0 = l[!grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("type", "simulation", "libraries", "analysis"), function(x){
  if(mean(x$cover) >= level) return(NULL)
  x
})
pl = ggplot(l1) + 
  geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
  facet_wrap(as.formula("~type"), scales = "free_y", labeller = label_parsed) + 
  geom_abline(aes(slope = 0, intercept = l1$truth)) + 
  xlab("simulated dataset") +
  ylab("credible interval") + 
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_hypercoverage, "fig_hypercoverage.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:betarates
dir_betarates = newdir(paste0(dir, "fig_betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("type", "rep"), function(x){
  data.frame(type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage")) + 
  geom_hline(yintercept = level) + 
  facet_wrap(as.formula("~type"), labeller = label_parsed) +
  xlab("simulated dataset") +
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betarates, "fig_betarates.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:hphcalibration
dir_hphcalibration = newdir(paste0(dir, "fig_hphcalibration"))
df = ggplot2_df("~/home/work/projects/thesis_data/results/coverage_analyze/calibration")
df = df[df$heterosis == "high-parent_hybrid1",]
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file", linetype = "analysis"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  mytheme_pub() + theme(legend.position = "none")
for(extn in extns)
  ggsave(paste0(dir_hphcalibration, "fig_hphcalibration.", extn), pl, height = 4, width = 4, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "fig_betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0 = l0[order(l0$truth),]
l0 = ddply(l0, "type", function(x){
  x$interval = 1:dim(x)[1]
  x
})
pl = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = "darkGray") +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("parameter value") + 
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig_betacred.", extn), pl, height = 6, width = 7, dpi = 1200)

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "fig_betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 0.1)
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
}, .progress = "text")
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep")) + 
  geom_abline(slope = 0, intercept = level) +
  facet_wrap(as.formula("~type"), scales = "free_x", labeller = label_parsed) +
  xlab("parameter value") +
  ylab("coverage") +
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacoveragetrend, "fig_betacoveragetrend.", extn), pl, height = 6, width = 7, dpi = 1200)

# fig:roc16 and fig:roc32
for(N in c(16, 32)){
  dir_roc = newdir(paste0(dir, "fig_roc", N))
  d = readRDS("comparison_analyze/plot_roc/roc.rds")
  ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
  d = d[d$analysis %in% ans & d$libraries == N,]
  d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
  d$analysis = myrelevel(d$analysis)
  d$analysis = ordered(d$analysis, levels = c("fully Bayes", "eBayes (naive)", "eBayes (posterior)", "eBayes (oracle)", "Niemi"))
  d$heterosis = relevel_heterosis(d$heterosis)

  pl = ggplot(d) + 
    geom_line(aes_string(x = "fpr", y = "tpr", group = "file", linetype = "analysis"), color = "black") +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("False positive rate") + 
    ylab("True positive rate") +
    labs(linetype = "Analysis") +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_roc, "fig_roc", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:auc16 and fig:auc32
dir_auc = newdir(paste0(dir, "fig_auc"))
d = readRDS("comparison_analyze/plot_auc/auc.rds")
ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
d = d[d$analysis %in% ans,]
d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
d$analysis = myrelevel(d$analysis)
d$analysis = ordered(d$analysis, levels = c("eBayes (oracle)", "eBayes (naive)", "eBayes (posterior)", "fully Bayes", "Niemi"))
d$heterosis = relevel_heterosis(d$heterosis)

pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "free_y") +
  xlab("Analysis") + 
  ylab("Area under ROC curve") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_auc, "fig_auc.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:comparecal
for(N in c(16, 32)){
  dir_comparecal = newdir(paste0(dir, "fig_comparecal", N))
  d = readRDS("comparison_analyze/plot_calibration/calibration.rds")
  ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
  d = d[d$analysis %in% ans & d$libraries == N,]
  d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
  d$analysis = myrelevel(d$analysis)
  d$analysis = ordered(d$analysis, levels = c("fully Bayes", "eBayes (naive)", "eBayes (posterior)", "eBayes (oracle)", "Niemi"))
  d$heterosis = relevel_heterosis(d$heterosis)

  pl = ggplot(d) + 
    geom_abline(slope = 1, intercept = 0) +
    geom_line(aes_string(x = "probability", y = "proportion", group = "file", linetype = "analysis"), color = "black") +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("Probability") + 
    ylab("Proportion") +
    labs(linetype = "Analysis") +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_comparecal, "fig_comparecal", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# paschold data analysis
dir_hyperhist = newdir(paste0(dir, "fig_hyperhist"))
l = readRDS("real_mcmc/paschold_39656_16_1.rds")
a = l$analyses[["fullybayes+normal"]]
m = mcmc_samples(a$chains)

# fig:hyperhist
dir_hyperhist = newdir(paste0(dir, "fig_hyperhist"))
m_hyper = m[,c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]
cn = colnames(m_hyper)
for(i in 1:5) cn = gsub(paste0("_", i), paste0("\\[", i, "\\]"), cn)
cn = gsub("sigmaSquared", "sigma", cn)
cn[grep("sigma", cn)] = paste0(cn[grep("sigma", cn)], "^2")
colnames(m_hyper) = cn
d = melt(m_hyper, id.vars = NULL)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value"), color = "black", fill = "black") + 
  facet_wrap(as.formula("~variable"), scales = "free_x", labeller = label_parsed) + 
  mytheme_pub() +
  theme(strip.text.x = element_text(size = 14), axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("Parameter value") + 
  ylab("Count")
for(extn in extns)
  ggsave(paste0(dir_hyperhist, "fig_hyperhist.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:betahist

dir_betahist = newdir(paste0(dir, "fig_betahist"))
m_beta = m[,grep("beta", colnames(m))]
cn = colnames(m_beta)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[list(", x[3],"~", x[2], ")]")
})
colnames(m_beta) = c2
d = melt(m_beta, id.vars = NULL)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value"), color = "black", fill = "black") + 
  facet_wrap(as.formula("~variable"), scales = "free_x", labeller = label_parsed) + 
  mytheme_pub() +
  theme(strip.text.x = element_text(size = 14), axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("Parameter value") + 
  ylab("Count")
for(extn in extns)
  ggsave(paste0(dir_betahist, "fig_betahist.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:gammahist
dir_gammahist = newdir(paste0(dir, "fig_gammahist"))
m_gamma = m[,grep("gamma", colnames(m))]
cn = colnames(m_gamma)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[", x[2],"]")
})
colnames(m_gamma) = c2
d = melt(m_gamma, id.vars = NULL)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value"), color = "black", fill = "black") + 
  facet_wrap(as.formula("~variable"), ncol = 2, scales = "free_x", labeller = label_parsed) + 
  mytheme_pub() +
  theme(strip.text.x = element_text(size = 14), axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("Parameter value") + 
  ylab("Count")
for(extn in extns)
  ggsave(paste0(dir_gammahist, "fig_gammahist.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:pascholdcred
dir_pascholdcred = newdir(paste0(dir, "fig_pascholdcred"))
e = estimates(a$chains)
e = e[grep("beta_", rownames(e)),]
e$parameter = gsub("_[0-9]*$", "", rownames(e))
s = do.call(rbind, strsplit(e$parameter, "_"))
ns = apply(s, 1, function(x){
  paste0(x[1], "[", x[2], "]")
})
e$parameter = ordered(ns, levels = paste0("beta[", 1:5, "]"))
d = ddply(e, "parameter", function(x){
  x = x[sample(dim(x)[1], 1e3),]
  x = x[order(x$mean),]
  x$index = 1:dim(x)[1]
  x
})
pl = ggplot(d) + 
  facet_wrap(as.formula("~parameter"), scales = "free", labeller = label_parsed) + 
  geom_segment(aes_string(x = "index", xend = "index", y = "lower_ci_0.95", yend = "upper_ci_0.95"), 
    color = "darkGray") + 
  geom_point(aes_string(x = "index", y = "mean"), size = I(0.15)) + 
  xlab("Index") +
  ylab("Parameter value") +
  mytheme_pub() + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0), strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_pascholdcred, "fig_pascholdcred.", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:probhist
dir_probhist = newdir(paste0(dir, "fig_probhist"))
p = as.data.frame(probs(a$chains))
d = melt(p, id.vars = NULL)
d$variable = relevel_heterosis_paschold(d$variable)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value"), color = "black", fill = "black") + 
  facet_wrap(as.formula("~variable"), scales = "free_x") + 
  xlab("Probability") + 
  ylab("Count") +
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_probhist, "fig_probhist.", extn), pl, height = 6, width = 8, dpi = 1200)

# tables of interesting genes


# supplementary table of heterosis probabilities
dir_suppheteroisisprobs = newdir(paste0(dir, "supp_suppheteroisisprobs"))
p = as.data.frame(probs(a$chains))
p$geneID = rownames(p)
data(paschold)
paschold = get("paschold")
ct = paschold@counts %*% kronecker(diag(4), matrix(1, nrow = 4))/4
ct = cbind(ct, paschold@counts)
colnames(ct) = c(paste0(unique(gsub("_[0-9]$", "", colnames(paschold@counts))), "_mean_count"), colnames(paschold@counts))
p = cbind(p, ct)
d = melt(p, id.vars = c("geneID", colnames(ct)))
d = d[,c(23, 22, 1:21)]
colnames(d)[1:2] = c("heterosis_prob", "heterosis_type")
d$heterosis_type = relevel_heterosis_paschold(d$heterosis_type)
d = d[order(d$heterosis_prob, decreasing = T),]
write.csv(d, paste0(dir_suppheteroisisprobs, "suppheterosisprobs.csv"), row.names = F)
}
