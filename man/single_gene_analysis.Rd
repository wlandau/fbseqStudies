% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_Niemi.R
\name{single_gene_analysis}
\alias{single_gene_analysis}
\title{Function \code{single_gene_analysis}}
\usage{
single_gene_analysis(x, group, hyperparameters, model)
}
\arguments{
\item{x}{RNA-seq counts for a single gene}

\item{group}{genetic varieties of columns of \code{counts}}

\item{hyperparameters}{list of hyperparameters provided by \code{get_hyperparameters}}

\item{model}{compiled \code{rstan} model}
}
\value{
internal object
}
\description{
Fit rstan to a single gene
}

