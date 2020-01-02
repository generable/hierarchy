

#' Inefficiently transpose a sparse matrix
#' 
#' @param m sparse matrix
#' @return transpose of m
st = function(m) Matrix::Matrix(t(as.matrix(m)))

#' Get non-zero entries of a matrix in a list of one item per row/column
get_nze = function(m, compression = "row") {
  if (compression == "row") {
    margin = 1
  } else if (compression == "column") {
    margin = 2
  } else {
    stop("Only 'row', and 'column' compression are recognized.")
  }
  nze = apply(m, margin, function(x) which(x != 0))
  if (!is.list(nze)) {
    if (is.null(dim(nze)))
      nze = as.list(nze)
    else
      nze = apply(nze, 2, list) 
      nze = lapply(nze, unlist)
  }
  return(nze)
}

get_start_stop_skip = function(nze) {
  n_col = length(nze)
  stop = sapply(nze, length)
  skip = which(stop == 0)
  stop = cumsum(stop)
  start = 1
  if (length(stop) > 1)
    start = c(start, stop[1:(n_col-1)] + 1)
  stop[skip] = 0
  start[skip] = 0
  return(list(start = start, stop = stop, skip = skip))
}

get_nzv = function(m) {
  xv = apply(m, 1, function(x) x[x != 0])
  if (is.matrix(xv))
    xv = as.vector(xv)
  xv = unlist(xv)
  return(xv)
}

#' Turn a row-wize nze into a col-wise NZE and vice-versa.
#'
flip_nze = function(nze, n, space = "row") {
  if (space == "column")
    return(nze)
  o = vector('list', n)
  nze_idx = 0
  for (i in seq_along(nze)) {
    for (j in seq_along(nze[[i]])) {
      nze_idx = nze_idx + 1
      idx = nze[[i]][[j]]
      if (length(o) < idx) {
        o = c(o, vector('list', idx - length(o)))
      }
      if (space == "row") {
        o[[idx]] = c(o[[idx]], i)
      } else if (space == "nze") {
        o[[idx]] = c(o[[idx]], nze_idx)
      }
    }
  }
  return(o)
}

#' Process fixed effects model
#'
#' @param MatrixModels::model.Matrix object
#' @return list with model matrix components and labels
#' @export
m_as_list = function(m) {
  ## Calculate matrix entries
  row_nze = get_nze(m, compression = "row")
  row_sss = get_start_stop_skip(row_nze)
  col_nze = flip_nze(row_nze, ncol(m))
  col_xv_nze = flip_nze(row_nze, ncol(m), space = "nze")
  row_nze = unlist(row_nze)
  row_nzv = get_nzv(m)
  col_sss = get_start_stop_skip(col_nze)
  col_nze = unlist(col_nze)
  m_list_form = list(
    n_row = nrow(m), 
    n_col = ncol(m),
    n_nze = length(row_nze),
    nze = row_nze,
    skip = row_sss[['skip']],
    start = row_sss[['start']], 
    stop = row_sss[['stop']],
    xv = row_nzv,
    col_n_nze = length(col_nze),
    col_nze = col_nze,
    col_xv_nze = unlist(col_xv_nze),
    col_skip = col_sss[['skip']],
    col_start = col_sss[['start']],
    col_stop = col_sss[['stop']]
  )
  return(m_list_form)
}





