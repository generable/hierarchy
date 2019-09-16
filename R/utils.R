
## All of these functions were migrated from 02-setup-functions.R
## in the ramsey repo: 
## https://github.com/generable/ramsey/commit/f575ecb9df2f346e3275909fe2455aab13c8a2a1


#' extract_term_idx
#' (migrated from 02-setup.functions.R)
#' @export
extract_term_idx = function(fmm) {
  o = matrix(data = c(fmm$.term_start, fmm$.term_stop), ncol = 2)
  colnames(o) = c('start', 'stop')
  tl = fmm$.specifiers$term_list$rhs
  if (class(tl) == 'call')
    tl = list(tl)
  rownames(o) = lapply(tl, deparse)
  return(o)
}

#' extract_term_coefficient_names
#' (migrated from 02-setup.functions.R)
#' @export
extract_term_coefficient_names = function(fmm) {
  idx = extract_term_idx(fmm)
  tn = rownames(idx)
  o = list()
  for (i in 1:nrow(idx))
    o[[i]] = colnames(fmm$.model$matrix)[idx[i,1]:idx[i,2]]
  names(o) = tn
  return(o)
}

#' extract_re_idx
#' (migrated from 02-setup.functions.R)
#' @export
extract_re_idx = function(fmm) {
  if (fmm$.n_re == 0)
    return(NULL)
  o = matrix(data = c(fmm$.re_start, fmm$.re_stop), ncol = 2)
  colnames(o) = c('start', 'stop')
  rownames(o) = fmm$.re_names
  return(o)
}

#' extract_re_coefficient_names
#' (migrated from 02-setup.functions.R)
#' @export
extract_re_coefficient_names = function(fmm) {
  idx = extract_re_idx(fmm)
  if (is.null(idx))
    return(NULL)
  tn = rownames(idx)
  o = list()
  for (i in 1:nrow(idx))
    o[[i]] = colnames(fmm$.model$matrix)[idx[i,1]:idx[i,2]]
  names(o) = tn
  return(o)
}

