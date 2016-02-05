#' @include util-myrelevel.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{paper_case_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_case_figures = function(){

# library(fbseqStudies); library(reshape2); library(plyr); library(pracma); library(ggthemes); library(actuar); setwd("~/home/work/projects/thesis_data/results")

# control parms
dir = newdir("case_study_paper_figures")
extns = c("pdf", "ps", "eps")
mycolors = c("black", "blue", "red", "green", "purple")
analysislevels = c("eBayes (oracle)", "eBayes (naive)", "eBayes (posterior)", "Niemi", "fully Bayes")

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))

# fig:hypercoverage
dir_hypercoverage = newdir(paste0(dir, "fig-hypercoverage"))
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
  ggsave(paste0(dir_hypercoverage, "fig-hypercoverage.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:betarates
dir_betarates = newdir(paste0(dir, "fig-betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
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
  ggsave(paste0(dir_betarates, "fig-betarates.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:modelcalibration
dir_modelcalibration = newdir(paste0(dir, "fig-modelcalibration"))
df = ggplot2_df("~/home/work/projects/thesis_data/results/coverage_analyze/calibration")
df$heterosis = relevel_heterosis(df$heterosis)
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_wrap(as.formula("~heterosis")) + xlab("Probability") + ylab("Proportion") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelcalibration, "fig-modelcalibration.", extn), pl, height = 4, width = 4, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "fig-betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l0 = ddply(l0, "type", function(x){
  x$interval = 1:dim(x)[1]
  x
})
pl = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = "darkGray") +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) +
  xlab("Credible interval") + ylab("Parameter value") + 
  mytheme_pub() + theme(strip.text.x = element_text(size = 14))
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig-betacred.", extn), pl, height = 6, width = 7, dpi = 1200)

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "fig-betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
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
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, height = 6, width = 7, dpi = 1200)

# fig:roc16 and fig:roc32
for(N in c(16, 32)){
  dir_roc = newdir(paste0(dir, "fig-roc", N))
  d = readRDS("comparison_analyze/plot_roc/roc.rds")
  ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
  d = d[d$analysis %in% ans & d$libraries == N,]
  d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
  d$analysis = myrelevel(d$analysis)
  d$analysis = ordered(d$analysis, levels = analysislevels)
  d$heterosis = relevel_heterosis(d$heterosis)

  pl = ggplot(d) + 
    geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("False positive rate") + 
    ylab("True positive rate") +
    labs(linetype = "Analysis") +
    scale_color_manual(name = "Analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "Analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_roc, "fig-roc", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:auc16 and fig:auc32
dir_auc = newdir(paste0(dir, "fig-auc"))
d = readRDS("comparison_analyze/plot_auc/auc.rds")
ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
d = d[d$analysis %in% ans,]
d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
d$analysis = myrelevel(d$analysis)
d$analysis = ordered(d$analysis, levels = analysislevels)
d$heterosis = relevel_heterosis(d$heterosis)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("Analysis") + 
  ylab("Area under ROC curve") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_auc, "fig-auc.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:comparecal
for(N in c(16, 32)){
  dir_comparecal = newdir(paste0(dir, "fig-comparecal", N))
  d = readRDS("comparison_analyze/plot_calibration/calibration.rds")
  ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
  d = d[d$analysis %in% ans & d$libraries == N,]
  d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
  d$analysis = myrelevel(d$analysis)
  d$analysis = ordered(d$analysis, levels = analysislevels)
  d$heterosis = relevel_heterosis(d$heterosis)

  pl = ggplot(d) + 
    geom_abline(slope = 1, intercept = 0) +
    geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("Probability") + 
    ylab("Proportion") +
    scale_color_manual(name = "Analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "Analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
    labs(linetype = "Analysis") +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_comparecal, "fig-comparecal", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:comparecalerror
dir_comparecalerror = newdir(paste0(dir, "fig-comparecalerror"))
df = readRDS("comparison_analyze/plot_calibration/calibration.rds")
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
ans = c("Niemi", as.character(analyses()[grep("+normal", analyses())]))
d = d[d$analysis %in% ans,]
d$simulation = ordered(d$simulation, levels = c("simple", "model", "edgeR", "Niemi"))
d$analysis = myrelevel(d$analysis)
d$analysis = ordered(d$analysis, levels = analysislevels)
d$heterosis = relevel_heterosis(d$heterosis)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("Analysis") + 
  ylab("Average absolute calibration error") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparecalerror, "fig-comparecalerror.", extn), pl, height = 8, width = 10, dpi = 1200)

# paschold data analysis
l = readRDS("real_mcmc/paschold_39656_16_1.rds")
a = l$analyses[["fullybayes+normal"]]
m = mcmc_samples(a$chains)
e = estimates(a$chains, level = 0.95)

# fig:hyperhist
dir_hyperhist = newdir(paste0(dir, "fig-hyperhist"))
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
  ggsave(paste0(dir_hyperhist, "fig-hyperhist.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:betahist
dir_betahist = newdir(paste0(dir, "fig-betahist"))
m_beta = m[,grep("beta", colnames(m))]
cn = colnames(m_beta)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[list(", x[3],"~~", x[2], ")]")
})
colnames(m_beta) = c2
d = melt(m_beta, id.vars = NULL)
d = ddply(d, "variable", function(x){
  x$lower = quantile(x$value, 0.025)
  x$upper = quantile(x$value, 0.975)
  x$median = quantile(x$value, 0.5)
  x$max = max(x$value)
  x$min = min(x$value)
  x
})
rownames(cn) = c2

ci1 = ddply(d, "variable", function(x){x[1,]})
ci2 = e[apply(cn, 1, paste0, collapse="_"),]
ci2$variable = ci1$variable
rownames(ci2) = ci2$variable
ci2$min = ci1$min
ci2$max = ci1$max

x1 = data.frame(variable = ci1$variable, lower = ci1$lower, upper = ci1$upper, center = ci1$median, Interval = "quantile", y = 1)
x2 = data.frame(variable = ci2$variable, lower = ci2$lower_ci_0.95, upper = ci2$upper_ci_0.95, center = ci2$mean, Interval = "normal approximation", y = 2)
ci = rbind(x1, x2)
ci$mean = c(ci2$mean, ci2$mean)
ci$sd = c(ci2$sd, ci2$sd)
ci$y = -ci$y/(15*sqrt(2 *pi * ci$sd^2))
ci$lb = ci$y - abs(ci$y*0.15)

nrm = ddply(ci2, "variable", function(x){
  value = seq(from = x$min[1], to = x$max[1], length.out = 100)
  norm = dnorm(value, x$mean[1], x$sd[1])
  data.frame(x[rep(1, 100),], value = value, norm = norm)
})

pl = ggplot() + 
  stat_density(data = d, mapping = aes_string(x = "value", y = "..density.."), color = "darkGray", fill = "darkGray") + 
  geom_line(data = nrm, mapping = aes_string(x = "value", y = "norm"), linetype = 2) +
  geom_errorbarh(data = ci, mapping = aes_string(x = "center", y = "y", xmin="lower", xmax="upper", linetype = "Interval"), height = 0) +
  geom_point(data = ci, mapping = aes_string(x = "lower", y = "y"), pch = 15, size = 0.18) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "y"), pch = 15, size = 0.18) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "lb"), alpha = 0, size = 0) +
  facet_wrap(as.formula("~variable"), ncol = 3, scales = "free", labeller = label_parsed) + 
  mytheme_pub() + 
  theme(strip.text.x = element_text(size = 14), legend.position = "none") + #, axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("Parameter value") + 
  ylab("Density") # + labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_betahist, "fig-betahist.", extn), pl, height = 10, width = 10, dpi = 1200)

# fig:gammahist
dir_gammahist = newdir(paste0(dir, "fig-gammahist"))
m_gamma = m[,grep("gamma", colnames(m))]
cn = colnames(m_gamma)
cn = do.call(rbind, strsplit(cn, "_"))
c2 = apply(cn, 1, function(x){
  paste0(x[1], "[", x[2],"]")
})
colnames(m_gamma) = c2
d = melt(m_gamma, id.vars = NULL)




d = ddply(d, "variable", function(x){
  x$lower = quantile(x$value, 0.025)
  x$upper = quantile(x$value, 0.975)
  x$median = quantile(x$value, 0.5)
  x$max = max(x$value)
  x$min = min(x$value)
  x
})
rownames(cn) = c2

ci1 = ddply(d, "variable", function(x){x[1,]})
ci2 = e[apply(cn, 1, paste0, collapse="_"),]
ci2$variable = ci1$variable
rownames(ci2) = ci2$variable
ci2$min = ci1$min
ci2$max = ci1$max

x1 = data.frame(variable = ci1$variable, lower = ci1$lower, upper = ci1$upper, center = ci1$median, Interval = "quantile", y = 1)
x2 = data.frame(variable = ci2$variable, lower = ci2$lower_ci_0.95, upper = ci2$upper_ci_0.95, center = ci2$mean, Interval = "inverse-gamma approximation", y = 2)
ci = rbind(x1, x2)
ci$mean = c(ci2$mean, ci2$mean)
ci$sd = c(ci2$sd, ci2$sd)
m = ci$mean
s = ci$sd
shape = m^2/s^2 + 2
scale = m*(shape - 1)
md = scale/(shape+1)
ci$y = -ci$y/(23*md)
ci$lb = ci$y - abs(ci$y*0.15)

ig = ddply(ci2, "variable", function(x){
  value = seq(from = x$min[1], to = x$max[1], length.out = 100)
  m = x$mean[1]
  s = x$sd[1]
  shape = m^2/s^2 + 2
  scale = m*(shape - 1)
  ig = dinvgamma(value, shape = shape, scale = scale)
  data.frame(x[rep(1, 100),], value = value, ig)
})

pl = ggplot() + 
  stat_density(data = d, mapping = aes_string(x = "value", y = "..density.."), color = "darkGray", fill = "darkGray") + 
  geom_line(data = ig, mapping = aes_string(x = "value", y = "ig"), linetype = 2) +
  geom_errorbarh(data = ci, mapping = aes_string(x = "center", y = "y", xmin="lower", xmax="upper", linetype = "Interval"), height = 0) +
  geom_point(data = ci, mapping = aes_string(x = "lower", y = "y"), pch = 15, size = 0.18) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "y"), pch = 15, size = 0.18) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "lb"), alpha = 0, size = 0) +
  facet_wrap(as.formula("~variable"), ncol = 2, scales = "free", labeller = label_parsed) + 
  mytheme_pub() + 
  theme(strip.text.x = element_text(size = 14), legend.position = "none") + #, axis.text.x = element_text(angle = -80, hjust = 0)) + 
  xlab("Parameter value") + 
  ylab("Density") # + labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_gammahist, "fig-gammahist.", extn), pl, height = 4, width = 5, dpi = 1200)

# fig:pascholdcred
dir_pascholdcred = newdir(paste0(dir, "fig-pascholdcred"))
e = estimates(a$chains)
e = e[grep("beta_", rownames(e)),]
e$parameter = gsub("_[0-9]*$", "", rownames(e))
s = do.call(rbind, strsplit(e$parameter, "_"))
ns = apply(s, 1, function(x){
  paste0(x[1], "[list(g", x[2], ")]")
})
e$parameter = ordered(ns, levels = paste0("beta[list(g", 1:5, ")]"))
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
  ggsave(paste0(dir_pascholdcred, "fig-pascholdcred.", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:probhist
dir_probhist = newdir(paste0(dir, "fig-probhist"))
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
  ggsave(paste0(dir_probhist, "fig-probhist.", extn), pl, height = 6, width = 8, dpi = 1200)

# supplementary table of heterosis probabilities
dir_suppheteroisisprobs = newdir(paste0(dir, "supp-suppheteroisisprobs"))
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
write.csv(d, paste0(dir_suppheteroisisprobs, "supp-heterosisprobs.csv"), row.names = F)

# compare with paschold results
data(paschold)
paschold = get("paschold")
ct = paschold@counts
file = "tableS3part.txt"
url = "http://genome.cshlp.org/content/suppl/2012/09/17/gr.138461.112.DC1/TableS3.xlsx"
if(!file.exists(file)){
cat(paste("File", file, "not found.
That file is needed to reproduce the rest of the analysis, specifically the 
figures and tables that compare our results to those of the 2012 
Paschold study (Genome Research 22:12). The file", file, "should be a 
tab-delimited text file containing columns A through C rows 3 through 
39658 of Paschold's Supplementary Table S3 
(", url, "),
which corresponds to Paschold's Table 1 (classes 5-8, columns 
B73xMo17 and Mo17xB73). Ensure that", file, "also has the 
header \"Gene\tMxB\tBxM\" and place the file in
your current working directory. Then, rerun this function.
"))
return()
} else {
  groups = read.table(file, header = T, sep = "\t")
}

cn = c("high-parent_B73xMo17", "high-parent_Mo17xB73", "low-parent_B73xMo17", "low-parent_Mo17xB73")
p = as.data.frame(probs(a$chains)[,cn])
colnames(p) = c("hph_bm", "hph_mb", "lph_bm", "lph_mb")
p$Gene = rownames(p)
d = melt(p, id.vars = "Gene")

g2 = data.frame(
  hph_bm = groups$BxM %in% 5:6,
  hph_mb = groups$MxB %in% 5:6,
  lph_bm = groups$BxM %in% 7:8,
  lph_mb = groups$MxB %in% 7:8,
  Gene = rownames(p)
)
d2 = melt(g2, id.vars = "Gene")

colnames(d) = c("Gene", "Heterosis", "Probability")
d$Paschold = ifelse(d2$value, "discovery", "nondiscovery") 
levels(d$Heterosis) = c(
  "hph_bm" = "high B73xMo17", 
  "hph_mb" = "high Mo17xB73", 
  "lph_bm" = "low B73xMo17", 
  "lph_mb" = "low Mo17xB73")

# fig:comparehprobs
dir_comparehprobs = newdir(paste0(dir, "fig-comparehprobs"))
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "Probability"), color = "black", fill = "black") + 
  facet_grid(Paschold~Heterosis, scales = "free_y") + 
  ylab("Count") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparehprobs, "fig-comparehprobs.", extn), pl, height = 6, width = 8, dpi = 1200)

# tables of interesting genes
ntopgenes = 15
for(type in levels(d$Heterosis)){
  iden = gsub(" ", "-", paste0("tab-", type, "-highprob-nondiscoveries"))
  td = newdir(paste0(dir, iden))
  x = d[d$Heterosis == type,]
  no = x[x$Paschold == "nondiscovery",]
  no = no[order(no$Probability, decreasing = T),]
  nogenes = no$Gene[1:ntopgenes]
  noct = ct[nogenes,]
  nom = as.data.frame(t(apply(noct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
  nom = cbind(rownames(nom), no$Probability[1:ntopgenes], nom)
  colnames(nom) = c("Gene", "Probability", unique(gsub("_[0-9]", "", colnames(noct))))
  rownames(nom) = NULL
  str = print(xtable(nom), include.rownames=F, sanitize.text.function=function(x){x}, hline.after = 0)
  write(str, file = paste0(td, iden, ".tex"))

  iden = gsub(" ", "-", paste0("tab-", type, "-lowprob-nondiscoveries"))
  td = newdir(paste0(dir, iden))
  x = d[d$Heterosis == type,]
  no = x[x$Paschold == "nondiscovery",]
  no = no[order(no$Probability, decreasing = F),]
  nogenes = no$Gene[1:ntopgenes]
  noct = ct[nogenes,]
  nom = as.data.frame(t(apply(noct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
  nom = cbind(rownames(nom), no$Probability[1:ntopgenes], nom)
  colnames(nom) = c("Gene", "Probability", unique(gsub("_[0-9]", "", colnames(noct))))
  rownames(nom) = NULL
  str = print(xtable(nom), include.rownames=F, sanitize.text.function=function(x){x}, hline.after = 0)
  write(str, file = paste0(td, iden, ".tex"))

  iden = gsub(" ", "-", paste0("tab-", type, "-lowprob-discoveries"))
  td = newdir(paste0(dir, iden))
  x = d[d$Heterosis == type,]
  yes = x[x$Paschold == "discovery",]
  yes = yes[order(yes$Probability, decreasing = F),]
  yesgenes = yes$Gene[1:ntopgenes]
  yesct = ct[yesgenes,]
  yesm = as.data.frame(t(apply(yesct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
  yesm = cbind(rownames(yesm), yes$Probability[1:ntopgenes], yesm)
  colnames(yesm) = c("Gene", "Probability", unique(gsub("_[0-9]", "", colnames(yesct))))
  rownames(yesm) = NULL
  str = print(xtable(yesm), include.rownames=F, sanitize.text.function=function(x){x}, hline.after = 0)
  write(str, file = paste0(td, iden, ".tex"))

  iden = gsub(" ", "-", paste0("tab-", type, "-highprob-discoveries"))
  td = newdir(paste0(dir, iden))
  x = d[d$Heterosis == type,]
  yes = x[x$Paschold == "discovery",]
  yes = yes[order(yes$Probability, decreasing = T),]
  yesgenes = yes$Gene[1:ntopgenes]
  yesct = ct[yesgenes,]
  yesm = as.data.frame(t(apply(yesct, 1, function(x){tapply(x, rep(1:4, each = 4), mean)})))
  yesm = cbind(rownames(yesm), yes$Probability[1:ntopgenes], yesm)
  colnames(yesm) = c("Gene", "Probability", unique(gsub("_[0-9]", "", colnames(yesct))))
  rownames(yesm) = NULL
  str = print(xtable(yesm), include.rownames=F, sanitize.text.function=function(x){x}, hline.after = 0)
  write(str, file = paste0(td, iden, ".tex"))
}
}