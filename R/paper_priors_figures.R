#' @include util-analyses.R util-simulations.R util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{paper_priors_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_priors_figures = function(){

# library(fbseqStudies); library(xtable); library(reshape2); library(plyr); library(pracma); library(ggthemes); library(actuar); setwd("~/home/work/projects/thesis_data/results"); library(readr)

# control parms
dir = newdir("priors_study_paper_figures")
extns = c("pdf", "ps", "eps")
mycolors = c("black", "blue", "red")
gray = "#707070"
analyses_avoid = "edgeR|ibayes|independence"

# fig:msecomparison
dir_msecomparison = newdir(paste0(dir, "PAPER3fig-msecomparison"))
mse = readRDS("comparison_analyze/mse/mse.rds")
mse$analysis = priors_relevel_analyses(mse$analysis)
mse$simulation = relevel_simulations(mse$simulation)
colnames(mse)[grep("beta", colnames(mse))] = paste0("beta[list(g", 1:5, ")]")
mse = melt(mse, id.vars = colnames(mse)[!grepl("beta", colnames(mse))])
mse$libraries = ordered(mse$libraries, levels = c(16, 32))
lvl = c("normal", "Laplace", "t")
mse = mse[mse$simulation != "Niemi",]
mse = mse[mse$analysis %in% lvl,]
mse$analysis = ordered(mse$analysis, levels = lvl)

pl = ggplot(mse) + 
  geom_line(aes_string(x = "analysis", y = "value", group = "libraries", linetype = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "value", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~variable"), scales = "free_y",  labeller = label_parsed) +
  xlab("analysis method") + 
  ylab("mean squared error") +
  labs(pch = "N", linetype = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))

for(extn in extns)
  ggsave(paste0(dir_msecomparison, "fig-msecomparison.", extn), pl, height = 7, width = 9, dpi = 1200)

# fig:msecoverage
dir_msecoverage = newdir(paste0(dir, "PAPER3fig-msecoverage"))
mse = readRDS("coverage_analyze/mse/mse.rds")
mse$analysis = priors_relevel_analyses(mse$analysis)
mse$simulation = relevel_simulations(mse$simulation)
colnames(mse)[grep("beta", colnames(mse))] = paste0("beta[list(g", 1:5, ")]")
mse = melt(mse, id.vars = colnames(mse)[!grepl("beta", colnames(mse))])
mse$libraries = ordered(mse$libraries, levels = c(16, 32))
lvl = c("normal", "Laplace", "t")
mse = mse[mse$simulation != "Niemi",]
mse = mse[mse$analysis %in% lvl,]
mse$analysis = ordered(mse$analysis, levels = lvl)

pl = ggplot(mse) + 
  geom_line(aes_string(x = "analysis", y = "value", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "value"), color = "black") +
  facet_wrap(as.formula("~variable"), scales = "free_y",  labeller = label_parsed) +
  xlab("analysis method") + 
  ylab("mean squared error") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))

for(extn in extns)
  ggsave(paste0(dir_msecoverage, "fig-msecoverage.", extn), pl, height = 7, width = 9, dpi = 1200)

# fig:msecoverage
dir_msepriors = newdir(paste0(dir, "PAPER3fig-msepriors"))
mse = readRDS("priors_analyze/mse/mse.rds")
mse$analysis = gsub("normalnormal", "normal", mse$analysis)
mse$analysis = gsub("normalLaplace", "Laplace", mse$analysis)
mse$analysis = gsub("normalt", "t", mse$analysis)
mse$analysis = priors_relevel_analyses(mse$analysis)
mse$simulation = relevel_simulations(mse$simulation)
colnames(mse)[grep("beta", colnames(mse))] = paste0("beta[list(g", 1:2, ")]")
mse = melt(mse, id.vars = colnames(mse)[!grepl("beta", colnames(mse))])
mse = mse[mse$libraries == 8,]
lvl = c("normal", "Laplace", "t")
mse = mse[mse$simulation != "Niemi",]
mse = mse[mse$analysis %in% lvl,]
mse$analysis = ordered(mse$analysis, levels = lvl)

pl = ggplot(mse) + 
  geom_line(aes_string(x = "analysis", y = "value", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "value"), color = "black") +
  facet_grid(as.formula("variable~simulation"), scales = "free_y",  labeller = label_parsed) +
  xlab("analysis method") + 
  ylab("mean squared error") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))

for(extn in extns)
  ggsave(paste0(dir_msepriors, "fig-msepriors.", extn), pl, height = 7, width = 9, dpi = 1200)


# fig-priorshyperhist
dir_priorshyperhist = newdir(paste0(dir, "PAPER3fig-priorshyperhist"))
m_hyper = NULL
for(f in list.files("priors_mcmc")) if(as.integer(meta(f)["libraries"]) == 8 & as.integer(meta(f)["rep"]) == 1){
  print(f)
  l = readRDS(paste0("priors_mcmc/", f))
  l$analyses = l$analyses[!grepl(analyses_avoid, names(l$analyses))]
  for(a in l$analyses){
    m = mcmc_samples(a$chains)
    m = m[,grep("nu|tau|theta|sigma", colnames(m))]
    m_long = melt(m, id.vars = NULL)
    m_long$simulation = gsub("priors", "", l$simulation)
    a$analysis = gsub("normalnormal", "normal", a$analysis)
    a$analysis = gsub("normalLaplace", "Laplace", a$analysis)
    a$analysis = gsub("normalt", "t", a$analysis)
    m_long$analysis = gsub("fullybayes\\+", "", a$analysis)
    tr = l$scenario@supplement$truth
    for(n in c("simulation", "analysis")) m_long[[n]] = ordered(m_long[[n]], levels = priors_analyses())
    m_long = ddply(m_long, "variable", function(x){
      s = unlist(strsplit(as.character(x$variable[1]), "_"))
      if(length(s) > 1){
        x$truth = slot(tr, s[1])[as.integer(s[2])]
      } else {
        x$truth = slot(tr, s[1])
      }
      x
    })
    m_hyper = rbind(m_hyper, m_long)
  }
}
m_hyper$simulation = ordered(m_hyper$simulation, labels = paste(levels(m_hyper$simulation), "sim"))
m_hyper$analysis = ordered(m_hyper$analysis,  labels = paste(levels(m_hyper$analysis), "analysis"))
for(v in unique(m_hyper$variable)){
  d = m_hyper[m_hyper$variable == v,]
  pl = ggplot(d) +
    stat_density(aes_string(x = "value", y = "..density.."), size = 0.75) + 
    geom_vline(xintercept = d$truth[1]) + 
    facet_grid(as.formula("simulation~analysis"), scales = "free") +
    mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_priorshyperhist, "fig-priorshyperhist-", d$variable[1], ".", extn), pl, height = 8, width = 8, dpi = 1200)
}

# fig-priorshypercoverage
dir_priorshypercoverage = newdir(paste0(dir, "PAPER3fig-priorshypercoverage"))
l = as.data.frame(readRDS("priors_analyze/ci/ci.rds"))
l = l[!grepl(analyses_avoid, l$analysis),]
l$analysis = gsub("normalnormal", "normal", l$analysis)
l$analysis = gsub("normalLaplace", "Laplace", l$analysis)
l$analysis = gsub("normalt", "t", l$analysis)
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grepl("fullybayes", l$analysis) & grepl("nu|tau|theta|sigma", l$type) & l$libraries == 8,]
l$parameter = as.factor(as.character(l$parameter))
l$type = as.factor(as.character(l$type))
l$simulation = gsub("priors", "", as.character(l$simulation))
l$analysis = gsub("fullybayes\\+", "", as.character(l$analysis))
for(n in c("simulation", "analysis")) l[[n]] = ordered(l[[n]], levels = priors_analyses())
l$simulation = ordered(l$simulation, labels = paste(levels(l$simulation), "sim"))
l$analysis = ordered(l$analysis,  labels = paste(levels(l$analysis), "analysis"))
x = ddply(l, "parameter", function(d){
  pl = ggplot(d) + 
    geom_segment(data = d[d$level == 0.5,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper"), size = 2) + 
    geom_segment(data = d[d$level == 0.95,], mapping = aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
    facet_grid(as.formula("simulation~analysis"), scales = "free") + 
    geom_abline(intercept = d$truth[1], slope = 0) + 
    xlab("simulated dataset") +
    ylab("credible interval") + 
    mytheme_pub() + theme(legend.position = "none") +
    scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 2))
  for(extn in extns)
    ggsave(paste0(dir_priorshypercoverage, "fig-priorshypercoverage-", d$parameter[1], ".", extn), pl, height = 8, width = 8, dpi = 1200)
})

# credible interval info
l = as.data.frame(readRDS("priors_analyze/ci/ci.rds"))
l = l[!grepl(analyses_avoid, l$analysis),]
l$analysis = gsub("normalnormal", "normal", l$analysis)
l$analysis = gsub("normalLaplace", "Laplace", l$analysis)
l$analysis = gsub("normalt", "t", l$analysis)
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grep("fullybayes", l$analysis) & l$type == "beta[2]" & l$libraries == 8,]

# fig:priorsbetarates
dir_priorsbetarates = newdir(paste0(dir, "PAPER3fig-priorsbetarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep", "analysis", "simulation"), function(x){
  data.frame(simulation = x$simulation[1], analysis = x$analysis[1], type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
saveRDS(l1, paste0(dir_priorsbetarates, "priorsbetarates.rds"))
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation), "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis), "analysis"))
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage"), size = 0.75) + 
  geom_hline(yintercept = level) + 
  facet_grid(as.formula("simulation~analysis")) +
  xlab("simulated dataset") + ylab("coverage rate") +
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_priorsbetarates, "fig-priorsbetarates.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:priorsbetacred
dir_priorsbetacred = newdir(paste0(dir, "PAPER3fig-priorsbetacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l1 = ddply(l0, c("analysis", "simulation", "type"), function(x){
  x$interval = 1:dim(x)[1]
  x
})
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation)[1:length(unique(l1$simulation))], "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis)[1:length(unique(l1$analysis))], "analysis"))
pl = ggplot(l1) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(yintercept = 0) + 
  facet_grid(as.formula("simulation~analysis"), scales = "free") +
  xlab("credible interval") + ylab("parameter value") + 
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_priorsbetacred, "fig-priorsbetacred.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:priorsbetacoveragetrend
dir_priorsbetacoveragetrend = newdir(paste0(dir, "PAPER3fig-priorsbetacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("simulation", "analysis", "rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(simulation = z$simulation[1], analysis = z$analysis[1], truth = xs, cover = ys, type = z$type[1], rep = z$rep[1])
}, .progress = "text")
l1$simulation = gsub("priors", "", as.character(l1$simulation))
l1$analysis = gsub("fullybayes\\+", "", as.character(l1$analysis))
for(n in c("simulation", "analysis")) l1[[n]] = ordered(l1[[n]], levels = priors_analyses())
l1$simulation = ordered(l1$simulation, labels = paste(levels(l1$simulation)[1:length(unique(l1$simulation))], "sim"))
l1$analysis = ordered(l1$analysis,  labels = paste(levels(l1$analysis)[1:length(unique(l1$analysis))], "analysis"))
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep"), alpha = 0.5) + 
  geom_abline(slope = 0, intercept = level, linetype = "dotted") +
  geom_vline(xintercept = 0) + 
  facet_grid(as.formula("simulation~analysis"), scales = "free") +
  xlab("true parameter value") +
  ylab("local coverage rate") +
  mytheme_pub()
for(extn in extns[!grepl("ps", extns)])
  ggsave(paste0(dir_priorsbetacoveragetrend, "fig-priorsbetacoveragetrend.", extn), pl, height = 8, width = 8, dpi = 1200)
for(extn in extns[grepl("ps", extns)])
  ggsave(paste0(dir_priorsbetacoveragetrend, "fig-priorsbetacoveragetrend.", extn), pl, device=cairo_ps,
 height = 8, width = 8, dpi = 1200)

# fig:priorsmodelcalibration
dir_priorsmodelcalibration = newdir(paste0(dir, "PAPER3fig-priorsmodelcalibration"))
df = readRDS("priors_analyze/calibration_long/calibration_long.rds")
df = df[!grepl(analyses_avoid, df$analysis),]
df = df[df$heterosis == "high" & df$libraries == 8,]
df$simulation = gsub("priors", "", as.character(df$simulation))
df$analysis = gsub("fullybayes\\+", "", as.character(df$analysis))
for(n in c("simulation", "analysis")) df[[n]] = ordered(df[[n]], levels = priors_analyses())
df$simulation = ordered(df$simulation, labels = paste(levels(df$simulation), "sim"))
df$analysis = ordered(df$analysis,  labels = paste(levels(df$analysis), "analysis"))
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_grid(as.formula("simulation~analysis")) + 
  xlab("estimated posterior probability") + 
  ylab("proportion of genes with heterosis") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsmodelcalibration, "fig-priorsmodelcalibration.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:priorscomparecalerror
dir_priorscomparecalerror = newdir(paste0(dir, "PAPER3fig-priorscomparecalerror"))
df = readRDS("priors_analyze/calibration_long/calibration_long.rds")
df = df[!grepl(analyses_avoid, df$analysis),]
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror"), color = "black") +
  facet_grid(as.formula("~simulation"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("calibration error") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorscomparecalerror, "fig-priorscomparecalerror.", extn), pl, height = 7, width = 7, dpi = 1200)

# fig:priorsroc
dir_priorsroc = newdir(paste0(dir, "PAPER3fig-priorsroc"))
d = readRDS("priors_analyze/roc_long/roc_long.rds")
d = d[!grepl(analyses_avoid, d$analysis),]
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
d$analysis = ordered(d$analysis,  labels = paste(levels(d$analysis), "analysis"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "fpr", y = "tpr", group = "file")) +
  facet_grid(as.formula("simulation~analysis")) +
  xlab("false positive rate") + 
  ylab("true positive rate") +
  scale_color_manual(name = "analysis", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
  scale_linetype_manual(name = "analysis", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsroc, "fig-priorsroc.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:priorsauc
dir_priorsauc = newdir(paste0(dir, "PAPER3fig-priorsauc"))
d = readRDS("priors_analyze/auc_long/auc_long.rds")
d = d[!grepl(analyses_avoid, d$analysis),]
d = d[d$heterosis == "high" & d$libraries == 8,]
d$simulation = gsub("priors", "", as.character(d$simulation))
d$analysis = gsub("fullybayes\\+", "", as.character(d$analysis))
for(n in c("simulation", "analysis")) d[[n]] = ordered(d[[n]], levels = priors_analyses())
d$simulation = ordered(d$simulation, labels = paste(levels(d$simulation), "sim"))
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1"), color = "black") +
  facet_grid(as.formula("~simulation"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("area under ROC curve") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_priorsauc, "fig-priorsauc.", extn), pl, height = 7, width = 7, dpi = 1200)







## compare with case study

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grep("fullybayes", l$analysis),]

# fig:betarates
dir_betarates = newdir(paste0(dir, "PAPER3fig-betarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("type", "rep", "analysis"), function(x){
  data.frame(analysis = x$analysis[1], type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
saveRDS(l1, paste0(dir_betarates, "betarates.rds"))
l1$analysis = ordered(gsub("fullybayes\\+", "", as.character(l1$analysis)), levels = priors_analyses())
pl = ggplot(l1) +
  geom_point(aes_string(x = "rep", y = "coverage"), size = 0.75) + 
  geom_hline(yintercept = level) + 
  facet_grid(as.formula("type~analysis"), labeller = label_parsed) +
  xlab("simulated dataset") + ylab("coverage rate") + 
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_betarates, "fig-betarates.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:betacred
dir_betacred = newdir(paste0(dir, "PAPER3fig-betacred"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == 1 & !l$cover,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l0 = l0[order(l0$truth),]
l0 = ddply(l0, c("analysis", "type"), function(x){
  x$interval = 1:dim(x)[1]
  x
})
l0$analysis = ordered(gsub("fullybayes\\+", "", as.character(l0$analysis)), levels = priors_analyses())
hmean = rep(0, dim(l0)[1])
hmean[l0$type == "beta[list(g1)]"] = 3
hmean[l0$type == "beta[list(g2)]"] = 0
hmean[l0$type == "beta[list(g3)]"] = -0.007
hmean[l0$type == "beta[list(g4)]"] = -0.005
hmean[l0$type == "beta[list(g5)]"] = 0.008
l0$hmean = hmean
pl = ggplot(l0) +
  geom_segment(aes_string(x = "interval", xend = "interval", y = "lower", yend = "upper"), color = gray) +
  geom_point(aes_string(x = "interval", y = "truth"), color = "black", size = I(0.5)) + 
  geom_hline(aes_string(yintercept = "hmean")) +
  facet_grid(as.formula("type~analysis"), scales = "free", labeller = label_parsed) +
  xlab("credible interval") + ylab("parameter value") + 
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_betacred, "fig-betacred.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:betacoveragetrend
dir_betacoveragetrend = newdir(paste0(dir, "PAPER3fig-betacoveragetrend"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l0$type = gsub("\\[", "[list(g", l0$type)
l0$type = gsub("]", ")]", l0$type)
l1 = ddply(l0, c("analysis", "rep", "type"), function(z){
  k = ksmooth(x = z$truth, y = z$cover, bandwidth = 4*sd(z$truth))
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
  ys = fn(xs)
  data.frame(analysis = z$analysis[1], truth = xs, cover = ys, type = z$type[1], rep = z$rep[1])
}, .progress = "text")
l1$analysis = ordered(gsub("fullybayes\\+", "", as.character(l1$analysis)), levels = priors_analyses())
hmean = rep(0, dim(l1)[1])
hmean[l1$type == "beta[list(g1)]"] = 3
hmean[l1$type == "beta[list(g2)]"] = 0
hmean[l1$type == "beta[list(g3)]"] = -0.007
hmean[l1$type == "beta[list(g4)]"] = -0.005
hmean[l1$type == "beta[list(g5)]"] = 0.008
l1$hmean = hmean
pl = ggplot(l1) + 
  geom_line(aes_string(x = "truth", y = "cover", group = "rep"), alpha = 0.5) + 
  facet_grid(as.formula("analysis~type"), scales = "free", labeller = label_parsed) +
  geom_abline(slope = 0, intercept = level, linetype = "dotted") +
  geom_vline(aes_string(xintercept = "hmean")) +
  xlab("true parameter value") +
  ylab("local coverage rate") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns[!grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, height = 6, width = 8, dpi = 1200)
for(extn in extns[grepl("ps", extns)])
  ggsave(paste0(dir_betacoveragetrend, "fig-betacoveragetrend.", extn), pl, device=cairo_ps,
 height = 8, width = 8, dpi = 1200)

# fig:modelcalibration
dir_modelcalibration = newdir(paste0(dir, "PAPER3fig-modelcalibration"))
df = readRDS("coverage_analyze/calibration_long/calibration_long.rds")
df = df[!grepl(analyses_avoid, df$analysis),]
df$heterosis = relevel_heterosis(df$heterosis)
df$analysis = ordered(gsub("fullybayes\\+", "", as.character(df$analysis)), levels = priors_analyses())
pl = ggplot(df) +
  geom_line(aes_string(x = "probability", y = "proportion", group = "file"), color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  facet_grid(as.formula("heterosis~analysis")) +
  xlab("estimated posterior probability") + 
  ylab("proportion of genes with heterosis") +
  mytheme_pub() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelcalibration, "fig-modelcalibration.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:modelcomparecalerror 
dir_modelcomparecalerror = newdir(paste0(dir, "PAPER3fig-modelcomparecalerror"))
df = readRDS("coverage_analyze/calibration_long/calibration_long.rds")
df = df[!grepl(analyses_avoid, df$analysis),]
df$heterosis = relevel_heterosis(df$heterosis)
df$analysis = ordered(gsub("fullybayes\\+", "", as.character(df$analysis)), levels = priors_analyses())
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "rep"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror"), color = "black") +
  facet_wrap(as.formula("~heterosis"), ncol = 3) +
  xlab("analysis method") + 
  ylab("calibration error") +
  labs(pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_modelcomparecalerror, "fig-modelcomparecalerror.", extn), pl, height = 7, width = 7, dpi = 1200)

# fig:roc16 and fig:roc32
for(N in c(16, 32)){
  dir_roc = newdir(paste0(dir, "PAPER3fig-roc", N))
  d = readRDS("comparison_analyze/roc_long/roc_long.rds")
  d = d[!grepl(analyses_avoid, d$analysis),]
  d = d[d$libraries == N,]
  d = priors_clean_df(d)
  pl = ggplot(d) + 
    geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("false positive rate") + 
    ylab("true positive rate") +
    scale_color_manual(name = "analysis method", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "analysis method", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_roc, "fig-roc", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:auc
dir_auc = newdir(paste0(dir, "PAPER3fig-auc"))
d = readRDS("comparison_analyze/auc_long/auc_long.rds")
d = d[!grepl(analyses_avoid, d$analysis),]
d = priors_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "auc_1", group = "libraries", linetype = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "auc_1", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("area under ROC curve") +
  labs(linetype = "N", pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_auc, "fig-auc.", extn), pl, height = 8, width = 10, dpi = 1200)

# fig:comparecal16 and fig:comparecal32
for(N in c(16, 32)){
  dir_comparecal = newdir(paste0(dir, "PAPER3fig-comparecal", N))
  d = readRDS("comparison_analyze/calibration_long/calibration_long.rds")
  d = d[!grepl(analyses_avoid, d$analysis),]
  d = d[d$libraries == N,]
  d = priors_clean_df(d)
  pl = ggplot(d) + 
    geom_abline(slope = 1, intercept = 0, color = gray) +
    geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) +
    facet_grid(as.formula("simulation~heterosis")) +
    xlab("estimated posterior probability") + 
    ylab("proportion of genes with heterosis") +
    scale_color_manual(name = "analysis method", labels = levels(d$analysis), values = mycolors[1:length(levels(d$analysis))]) +
    scale_linetype_manual(name = "analysis method", labels = levels(d$analysis), values = 1:length(levels(d$analysis))) +
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0))
  for(extn in extns)
    ggsave(paste0(dir_comparecal, "fig-comparecal", N, ".", extn), pl, height = 8, width = 10, dpi = 1200)
}

# fig:comparecalerror
dir_comparecalerror = newdir(paste0(dir, "PAPER3fig-comparecalerror"))
df = readRDS("comparison_analyze/calibration_long/calibration_long.rds")
df = df[!grepl(analyses_avoid, df$analysis),]
df$error = abs(df$proportion - df$probability)
d = ddply(df, c("file", "heterosis"), function(x){
  x$meanerror = trapz(x = x$probability, y = x$error)
  x[1,]
})
d = priors_clean_df(d)
pl = ggplot(d) + 
  geom_line(aes_string(x = "analysis", y = "meanerror", group = "libraries", linetype = "libraries"), color = "black") +
  geom_point(aes_string(x = "analysis", y = "meanerror", pch = "libraries"), color = "black") +
  facet_grid(as.formula("simulation~heterosis"), scales = "fixed") +
  xlab("analysis method") + 
  ylab("calibration error") +
  labs(linetype = "N", pch = "N") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparecalerror, "fig-comparecalerror.", extn), pl, height = 8, width = 10, dpi = 1200)

# paschold data analysis
data(paschold)
paschold = get("paschold")
scaledown()
fname = paste0("real_mcmc/paschold_", nrow(paschold@counts),
  "_", ncol(paschold@counts), "_1.rds")
l = readRDS(fname)
l$analyses = l$analyses[!grepl(analyses_avoid, names(l$analyses))]
as = list(normal = l$analyses[["fullybayes+normal"]],
  Laplace = l$analyses[["fullybayes+Laplace"]],
  t = l$analyses[["fullybayes+t"]])
m = lapply(as, function(a) mcmc_samples(a$chains))
e = lapply(as, function(a) estimates(a$chains, level = 0.95))

#fig:logcounts
dir_logcounts = newdir(paste0(dir, "PAPER3fig-logcounts"))
data(paschold)
paschold = get("paschold")
scaledown()
d = melt(log(paschold@counts + 1))
pl = ggplot(d) + stat_density(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  mytheme_pub() + 
  xlab("log(count + 1)")
for(extn in extns)
  ggsave(paste0(dir_logcounts, "fig-logcounts.", extn), pl, height = 8, width = 8, dpi = 1200)

# fig:hyperhist
dir_hyperhist = newdir(paste0(dir, "PAPER3fig-hyperhist"))
d = NULL
for(prior in priors_analyses()){
  m_hyper = m[[prior]][,c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]
  cn = colnames(m_hyper)
  for(i in 1:5) cn = gsub(paste0("_", i), paste0("\\[", i, "\\]"), cn)
  cn = gsub("sigmaSquared", "sigma", cn)
  cn[grep("sigma", cn)] = paste0(cn[grep("sigma", cn)], "^2")
  colnames(m_hyper) = cn
  d0 = melt(m_hyper, id.vars = NULL)
  d0$analysis = prior
  d = rbind(d, d0)
}
d$analysis = ordered(d$analysis, levels = priors_analyses())
for(v in unique(d$variable)){ # c("nu|tau", "theta", "sigma")
  pl = ggplot(d[d$variable == v,]) + 
    stat_density(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
    facet_grid(as.formula("analysis~variable"), scales = "free", labeller = label_parsed) + 
    mytheme_pub() +
    theme(axis.text.x = element_text(angle = -80, hjust = 0)) + 
    xlab("parameter value") + 
    ylab("density")
  for(extn in extns)
    ggsave(paste0(dir_hyperhist, "fig-hyperhist-", v, ".", extn), pl, height = 7, width = 7, dpi = 1200)
}

# loop over type of hierarchical distribution
data(paschold)
paschold = get("paschold")
scaledown()
fname = paste0("real_mcmc/paschold_", nrow(paschold@counts),
  "_", ncol(paschold@counts), "_1.rds")
l = readRDS(fname)
l$analyses = l$analyses[!grepl(analyses_avoid, names(l$analyses))]
set.seed(10)
edger = fit_edgeR(paschold@counts, paschold@design)
for(prior in priors_analyses()){

a = l$analyses[[paste0("fullybayes+", prior)]]
m = mcmc_samples(a$chains)
e = estimates(a$chains, level = 0.95)
e0 = e

# fig:betahist
dir_betahist = newdir(paste0(dir, "PAPER3fig-betahist"))
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
  stat_density(data = d, mapping = aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  geom_line(data = nrm, mapping = aes_string(x = "value", y = "norm"), linetype = 2) +
  geom_errorbarh(data = ci, mapping = aes_string(x = "center", y = "y", xmin="lower", xmax="upper", linetype = "Interval"), height = 0) +
  geom_point(data = ci, mapping = aes_string(x = "lower", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "y"), size = 0.5) +
  geom_point(data = ci, mapping = aes_string(x = "upper", y = "lb"), alpha = 0, size = 0) +
  facet_wrap(as.formula("~variable"), scales = "free", labeller = label_parsed) + 
  mytheme_pub() + 
  theme(legend.position = c(0.9, 0.1),plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"), 
axis.text.x = element_text(angle = -80, hjust = 0)) +
  xlab("parameter value") + 
  ylab("density") + 
  labs(linetype = "95% credible interval")
for(extn in extns)
  ggsave(paste0(dir_betahist, "fig-betahist-", prior, ".", extn), pl, height = 10, width = 10, dpi = 1200)

# fig:betapostmeanhist
dir_betapostmeanhist = newdir(paste0(dir, "PAPER3fig-betapostmeanhist"))
e = estimates(a$chains)
e = e[grep("beta_", rownames(e)),]
e$parameter = gsub("_[0-9]*$", "", rownames(e))
s = do.call(rbind, strsplit(e$parameter, "_"))
ns = apply(s, 1, function(x){
  paste0(x[1], "[list(g", x[2], ")]")
})
e$parameter = ordered(ns, levels = paste0("beta[list(g", 1:5, ")]"))
pl = ggplot(e) + 
  facet_wrap(as.formula("~parameter"), scales = "free", labeller = label_parsed) + 
  stat_density(aes_string(x = "mean", y = "..density.."), color = gray, fill = gray) + 
  xlab("estimated posterior mean") +
  ylab("density") +
  mytheme_pub() + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_betapostmeanhist, "fig-betapostmeanhist-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:pascholdcred
dir_pascholdcred = newdir(paste0(dir, "PAPER3fig-pascholdcred"))
d = ddply(e, "parameter", function(x){
  x = x[sample(dim(x)[1], min(1e3, dim(x)[1])),]
  x = x[order(x$mean),]
  x$index = 1:dim(x)[1]
  x
})
pl = ggplot(d) + 
  facet_wrap(as.formula("~parameter"), scales = "free", labeller = label_parsed) + 
  geom_segment(aes_string(x = "index", xend = "index", y = "lower_ci_0.95", yend = "upper_ci_0.95"), 
    color = gray) + 
  geom_point(aes_string(x = "index", y = "mean"), size = I(0.15)) + 
  xlab("credible interval") +
  ylab("parameter value") +
  mytheme_pub() + 
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_pascholdcred, "fig-pascholdcred-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

# fig:probhist
dir_probhist = newdir(paste0(dir, "PAPER3fig-probhist"))
p = as.data.frame(probs(a$chains))
d = melt(p, id.vars = NULL)
d$variable = relevel_heterosis_paschold(d$variable)
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "value", y = "..density.."), color = gray, fill = gray) + 
  facet_wrap(as.formula("~variable"), scales = "free_x") + 
  xlab("estimated posterior probability") + 
  ylab("density") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_probhist, "fig-probhist-", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

# compare with paschold results
data(paschold)
paschold = get("paschold")
scaledown()
ct = paschold@counts
data(tableS3table1)
groups = get("tableS3table1")
scaledown()
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
paschold_status = d

# fig:comparehprobs
dir_comparehprobs = newdir(paste0(dir, "PAPER3fig-comparehprobs"))
pl = ggplot(d) + 
  geom_histogram(aes_string(x = "Probability", y = "..density.."), color = gray, fill = gray) + 
  facet_grid(Paschold~Heterosis, scales = "free_y") + 
  xlab("estimated posterior probability") +
  ylab("density") +
  mytheme_pub() +
  theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparehprobs, "fig-comparehprobs", prior, ".", extn), pl, height = 6, width = 8, dpi = 1200)

# Supplementary tables (like TableS1.csv from case study)
dir_suppTables = newdir(paste0(dir, "PAPER3suppTables"))
e = e0
p = as.data.frame(probs(a$chains))
geneID = rownames(p)
colnames(p) = paste0("probability_", colnames(p))
colnames(p) = gsub("parent", "parent-heterosis", colnames(p))
colnames(p) = gsub("B73xMo17_Mo17xB73", "hybrid-mean", colnames(p))
data(paschold)
paschold = get("paschold")
scaledown()
ct = paschold@counts
colnames(ct) = paste0(colnames(paschold@counts), "_count-data")
p = cbind(geneID, ct, p)

#highB73xMo17 = paschold_status[paschold_status$Heterosis == "high B73xMo17",]
#lowB73xMo17 = paschold_status[paschold_status$Heterosis == "low B73xMo17",]
#highMo17xB73 = paschold_status[paschold_status$Heterosis == "high Mo17xB73",]
#lowMo17xB73 = paschold_status[paschold_status$Heterosis == "low Mo17xB73",]
#rownames(highB73xMo17) = highB73xMo17$Gene
#rownames(lowB73xMo17) = lowB73xMo17$Gene
#rownames(highMo17xB73) = highMo17xB73$Gene
#rownames(lowMo17xB73) = lowMo17xB73$Gene
#p[["Paschold-results_high-parent-heterosis_B73xMo17"]] = highB73xMo17[p$geneID,"Paschold"]
#p[["Paschold-results_low-parent-heterosis_B73xMo17"]] = lowB73xMo17[p$geneID,"Paschold"]
#p[["Paschold-results_high-parent-heterosis_Mo17xB73"]] = highMo17xB73[p$geneID,"Paschold"]
#p[["Paschold-results_low-parent-heterosis_Mo17xB73"]] = lowMo17xB73[p$geneID,"Paschold"]

rownames(p) = NULL
for(i in 1:5){
  p[[paste0("beta_g", i, "_mean")]] = e[grepl(paste0("beta_", i), rownames(e)), "mean"]
  p[[paste0("beta_g", i, "_standard-deviation")]] = e[grepl(paste0("beta_", i), rownames(e)), "sd"]
}
p[["gamma_mean"]] = e[grepl("gamma", rownames(e)), "mean"]
p[["gamma_standard-deviation"]] = e[grepl("gamma", rownames(e)), "sd"]
parms_edger = cbind(edger$estimates[,grep("beta_", colnames(edger$estimates))], dispersion = edger$dispersion)
colnames(parms_edger) = paste0(colnames(parms_edger), "_edgeR_estimate")
for(i in 1:5) colnames(parms_edger) = gsub(paste0("beta_", i), paste0("beta_g", i), colnames(parms_edger))

effect_edger = edger$estimates[,grep("effect_", colnames(edger$estimates))]
colnames(effect_edger) = gsub("effect_", "", colnames(effect_edger))
colnames(effect_edger) = gsub("_parent", "-parent", colnames(effect_edger))
colnames(effect_edger) = gsub("hybrids", "hybrid-mean", colnames(effect_edger))
colnames(effect_edger) = gsub("hybrid1", "B73xMo17", colnames(effect_edger))
colnames(effect_edger) = gsub("hybrid2", "Mo17xB73", colnames(effect_edger))
colnames(effect_edger) = paste0(colnames(effect_edger), "_edgeR_effect_size")
scales = exp(parms_edger[,"dispersion_edgeR_estimate"])
effect_edger = sweep(effect_edger, 1, scales, "/")

effect_fullybayes = a$estimates[,grep("effect", colnames(a$estimates))]
colnames(effect_fullybayes) = colnames(effect_edger)
colnames(effect_fullybayes) = gsub("edgeR_", "", colnames(effect_fullybayes))
scales = sqrt(p[["gamma_mean"]])
effect_fullybayes = sweep(effect_fullybayes, 1, scales, "/")

p = cbind(p, effect_fullybayes, #effect_edger, 
  parms_edger)
write.csv(p, paste0(dir_suppTables, prior, ".csv"), row.names = F)

# fig-edgerparms
d = as.data.frame(read_csv(paste0(dir_suppTables, prior, ".csv")))
d$gamma_mean = log(d$gamma_mean)
d$dispersion_edgeR_estimate = log(d$dispersion_edgeR_estimate)
dir_edgerparms = newdir(paste0(dir, "PAPER3fig-edgerparms"))

parms_fb = melt(d[,grepl("mean", colnames(d)) & !grepl("hybrid", colnames(d))])
parms_edger = melt(d[,grepl("estimate", colnames(d)) & !grepl("hybrid", colnames(d))])
parms_fb$variable = as.character(parms_fb$variable)
parms_edger$variable = as.character(parms_edger$variable)
parms = parms_fb

x = parms$variable
x = gsub("_mean", "", x)
x = gsub("beta_g", "beta[list(g", x)
x = gsub("gamma", "gamma[list(g", x)
x = paste0(x, ")]")
x = gsub("gamma", "log(gamma", x)
x = gsub("g)]", "g)])~vs~log(disp)", x)
parms$variable = x
colnames(parms) = c("variable", "fullyBayes")
parms[["edgeR"]] = parms_edger$value

data(paschold)
paschold = get("paschold")
scaledown()
fname = paste0("real_mcmc/paschold_", nrow(paschold@counts),
  "_", ncol(paschold@counts), "_1.rds")
l = readRDS(fname)
l$analyses = l$analyses[!grepl(analyses_avoid, names(l$analyses))]
a = l$analyses[[paste0("fullybayes+", prior)]]
m = mcmc_samples(a$chains)
m2 = apply(m, 2, mean)
hmeans = data.frame(variable = unique(parms$variable), value = 0)
for(i in 1:5) hmeans$value[i] = m2[paste0("theta_", i)]
nu = m2["nu"]
tau = m2["tau"]
a = nu/2
b = tau*a
hmeans$value[6] = log(b) + digamma(a)

pl = ggplot(parms) +
  stat_binhex(aes_string(x = "edgeR", y = "fullyBayes"), bins = 35) +
  geom_abline(slope = 1) + 
  geom_hline(data = hmeans, mapping = aes_string(yintercept = "value")) +
  facet_wrap(as.formula("~variable"), labeller = label_parsed, scales = "free") +
  scale_fill_gradient(guide = F, name = "count", 
    trans = "log", low = "#b5b5b5", high = "black") +
  ylab("fully Bayes") +
  mytheme_pub()
for(extn in extns)
  ggsave(paste0(dir_edgerparms, "fig-edgerparms", prior, ".", extn), 
    pl, height = 7, width = 9, dpi = 1200)

# fig-volcano
d = as.data.frame(read_csv(paste0(dir_suppTables, prior, ".csv")))
dir_volcano = newdir(paste0(dir, "PAPER3fig-volcano"))
effects = melt(d[,grepl("geneID|effect", colnames(d))], id.vars = "geneID")
probs = melt(d[,grepl("geneID|probability", colnames(d))], id.vars = "geneID")
x = effects$variable
x = gsub("probability_|hybrid-|_effect_size|-heterosis", "", x)
x = gsub("-parent_", " ", x)
x = ordered(x, levels = 
  c("high B73xMo17", "high Mo17xB73", "high mean",
    "low B73xMo17", "low Mo17xB73", "low mean"))
df = data.frame(gene = effects$geneID, effect = effects$value, 
  probability = probs$value, heterosis = x)

pl = ggplot(df) +
  stat_binhex(aes_string(x = "effect", y = "probability"), bins = 35) +
  scale_fill_gradient(guide = F, name = "count", 
    trans = "log", low = "#b5b5b5", high = "black") +
  facet_wrap(as.formula("~heterosis"), scales = "free") +
  xlab("effect size") + ylab("posterior probability of heterosis") +
  mytheme_pub()

for(extn in extns)
  ggsave(paste0(dir_volcano, "fig-volcano", prior, ".", extn), 
    pl, height = 7, width = 9, dpi = 1200)

} # for(prior in priors_analyses())

# credible interval info for comparison study
l = as.data.frame(readRDS("comparison_analyze/ci/ci.rds"))
l = l[!grepl(analyses_avoid, l$analysis),]
l$rep = ordered(l$rep, levels = 1:max(as.integer(l$rep)))
l = l[grepl("fullybayes", l$analysis),]
l = l[!grepl("horseshoe", l$analysis),]
l = l[!grepl("horseshoe|Niemi", l$simulation),]

# fig:comparebetarates
dir_comparebetarates = newdir(paste0(dir, "PAPER3fig-comparebetarates"))
level = 0.95
l0 = l[grepl("beta", l$parameter) & l$level == level,]
l1 = ddply(l0, c("type", "rep", "simulation", "analysis", "libraries"), function(x){
  data.frame(type = x$type[1], rep = x$rep[1], coverage = mean(x$cover))
})
l1$analysis = ordered(gsub("fullybayes\\+", "", as.character(l1$analysis)), levels = priors_analyses())
l1$simulation = relevel_simulations(l1$simulation)
l1$type = gsub("\\[", "[list(g", l1$type)
l1$type = gsub("]", ")]", l1$type)
saveRDS(l1, paste0(dir_comparebetarates, "comparebetarates.rds"))
pl = ggplot(l1) +
  geom_hline(yintercept = level, color = gray) + 
  geom_line(aes_string(x = "analysis", y = "coverage", group = "libraries", linetype = "libraries")) + 
  geom_point(aes_string(x = "analysis", y = "coverage", pch = "libraries")) + 
  facet_grid(as.formula("simulation~type"), labeller = label_parsed) +
  xlab("analysis method") + ylab("coverage rate") + 
  labs(linetype = "N", pch = "N") +
  mytheme_pub() + theme(axis.text.x = element_text(angle = -80, hjust = 0))
for(extn in extns)
  ggsave(paste0(dir_comparebetarates, "fig-comparebetarates.", extn), pl, height = 7, width = 7, dpi = 1200)

} # paper_priors_figures

