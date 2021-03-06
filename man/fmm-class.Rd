% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat-model-matrix.R
\docType{class}
\name{fmm-class}
\alias{fmm-class}
\alias{fmm_factory}
\title{A class for creating a flat sparse model matrix
representation}
\description{
A class for creating a flat sparse model matrix
representation
}
\section{Fields}{

\describe{
\item{\code{.n_row}}{number of observations in model matrix}

\item{\code{.n_col}}{number of columns in the model matrix}

\item{\code{.n_nze}}{number of non-zero entries in the model matrix}

\item{\code{.nze}}{indexes of non-zero entries in the model matrix (RSC)}

\item{\code{.start}}{where each row of the model matrix start in nze}

\item{\code{.stop}}{where each row of the model matrix ends in nze}

\item{\code{.xv}}{n_nze nze entries of the model matrix}

\item{\code{.y}}{dependent data vector, if applicable}

\item{\code{.groups}}{index into xv for each group of parameters produced by the formula}
}}

\section{Methods}{

\describe{
\item{\code{check_component(component)}}{Verify that the requested (formula) component is 
in the model matrix and return its name.  If none
is specified (NULL in calling function) then all 
are returned.}

\item{\code{expose(...)}}{Extractor that takes a named vector and provides the relevant
       component with (optionally) a new name.  The renaming syntax follows
       dplyr::rename so the new name is taken from the name of the argument and
       the element to extract is taken from the character vector content.

       For example OBJ$expose(phi_n_col = 'n_col') would return the number of (implicit)
       model matrix columns with the name 'phi_n_col'.  This is useful to construct
       lists that are going to be used in, e.g.-Stan.}

\item{\code{get_data()}}{Get the data frame used to construct the matrix.}

\item{\code{initialize(formula, data, configuration, N = nrow(data), ...)}}{Create the implicit mass matrix and store components.}

\item{\code{list_components()}}{Get a list of the object's fields that can be exposed.}
}}

