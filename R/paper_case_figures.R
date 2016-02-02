#' @title Function \code{paper_case_figures}
#' @description Reproduce the figures and tables of the case study paper
#' @export
paper_case_figures = function(){

# setwd("~/home/work/projects/thesis_data/results")

# credible interval info
l = as.data.frame(readRDS("coverage_analyze/ci/ci.rds"))
dir = newdir("case_study_paper_figures")

# fig:hypercoverage
dir_hypercoverage = newdir(paste0(dir, "hypercoverage"))
level = 0.5
l0 = l[l$parameter %in% parms & l$level == level,]
l1 = ddply(l0, c("parameter", "simulation", "libraries", "analysis"), function(x){
  if(mean(l1$cover) >= level) return(NULL)
  x
})
pl = ggplot(l1) + 
  geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
  facet_wrap(as.formula("~type"), scales = "free_y", labeller = label_parsed) + 
  geom_abline(aes(slope = 0, intercept = l1$truth)) + 
  xlab("simulated dataset") +
  ylab("credible interval") + 
  theme_few() + theme(strip.text.x = element_text(size = 14))
ggsave(paste0(dir_hypercoverage, "hypercoverage.pdf"), pl, height = 4, width = 5, dpi = 1200)
ggsave(paste0(dir_hypercoverage, "hypercoverage.ps"), pl, height = 4, width = 5, dpi = 1200)
ggsave(paste0(dir_hypercoverage, "hypercoverage.eps"), pl, height = 4, width = 5, dpi = 1200)
ggsave(paste0(dir_hypercoverage, "hypercoverage.tiff"), pl, height = 4, width = 5, dpi = 1200)

# fig:betarates


}
