% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hierarchy.R
\docType{class}
\name{hierarchy-class}
\alias{hierarchy-class}
\alias{hierarchy_factory}
\title{A class for indexing a hierarchy in a model}
\description{
The class holds hierarchical relationships over all timepoints, and can
return indexes for any given level and all parents.
}
\section{Fields}{

\describe{
\item{\code{.levels}}{names of the (child) levels in the hierarchy}

\item{\code{.table}}{table representing the hierarchy}
}}

\section{Methods}{

\describe{
\item{\code{get_hierarchy_idxs(timepoint = NULL)}}{Get a matrix with the indexes of all levels of the hierarchy, 
with onoe entry per bottom-level node.  This format produces
redundancy at the higher levels (a given higher-level parameter
will appear multiple times) because that allows it to represent
the hierarchy clearly even when not all parts have the same number
of levels.}

\item{\code{get_level_idxs(level_name, timepoint = NULL)}}{Get the indexes for all parameters at a given level.}

\item{\code{get_parent_idxs(idxs, timepoint = NULL)}}{Get the indexes of the parents of a given set of indexes.  This is 
relevant if the hierarchy is not cleanly split into K levels for all
indexes.}

\item{\code{get_size(timepoint = NULL)}}{Get the number of parameters in the _entire_ hierarchy at a given timepoint, including the zero parameter.}

\item{\code{get_table(timepoint = NULL)}}{Method to retrieve the state of the hierarchy at a given point in time (date)}

\item{\code{get_vectorized_hierarchy(timepoint = NULL)}}{Get a list of three vectors: 1) the matrix of all hierarchy
indexes collapsed into a single vector; 2) the location in 
the vector where each level start; and 3) the location (index)
in the vector where each level ends.}

\item{\code{initialize(files, ...)}}{Construct hierarchy from given relationship files.}
}}

