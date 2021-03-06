% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linreg.R
\docType{class}
\name{linreg-class}
\alias{linreg-class}
\alias{linreg}
\title{RC-class linreg}
\description{
RC-class linreg
}
\section{Fields}{

\describe{
\item{\code{dep}}{numeric.}

\item{\code{indep}}{matrix.}

\item{\code{betahat}}{numeric.}

\item{\code{predicted}}{matrix.}

\item{\code{residuals}}{matrix.}

\item{\code{inv_sqr_dep}}{matrix.}

\item{\code{deg_fre}}{numeric.}

\item{\code{sigmasq}}{numeric.}

\item{\code{std_e}}{numeric.}

\item{\code{var_betas}}{numeric.}

\item{\code{se_betas}}{numeric.}

\item{\code{t_values}}{numeric.}

\item{\code{p_values}}{numeric.}

\item{\code{data}}{data.frame.}

\item{\code{formula}}{formula.}

\item{\code{dataname}}{character.}

\item{\code{output}}{data.frame.}
}}

\section{Methods}{

\describe{
\item{\code{coef()}}{Method coef}

\item{\code{initialize(form, data)}}{Formula for reggression}

\item{\code{plot()}}{plots the residuals versus fitted}

\item{\code{pred()}}{Method for prediction}

\item{\code{tvalue()}}{method t-value}
}}

