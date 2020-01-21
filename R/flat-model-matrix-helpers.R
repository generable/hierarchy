

#' Inefficiently transpose a sparse matrix
#'
#' Also transparently forwards to `t` for a dense matrix
#' 
#' @param m sparse matrix
#' @return transpose of m
st = function(m) {
  if (class(m) != 'matrix')
    m = as.matrix(m)
  m = Matrix::Matrix(t(m))
  return(m)
}

#' Get non-zero entries of a matrix in a list of one item 
#' per row/column
#'
#' @param m matrix (usable by base `apply`)
#' @param compression either 'row' (list will have one item
#'        per row) or 'column' (list will have one vector
#'        per column).
#' @return if compression is 'row' list will have one item
#'        per row.  If compression is 'column' (list will 
#'        have one vector per column).
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


is_same_float <- function(xv, nze, last_start, last_stop, start, stop, tol = 1e-06) {
  j <- start
  if (last_start == 0 && last_stop == 0) {
    if (start != 0) {
      return(0)
    } else {
      return(1)
    }
  }
  for (i in last_start:last_stop) {
    if (abs(xv[i] - xv[j]) > tol)
      return(0)
    j = j + 1
  }
  return(1)
}

is_same_int <- function(nze, last_start, last_stop, start, stop) {
  j <- start
  if (last_start == 0 && last_stop == 0) {
    if (start != 0) {
      return(0)
    } else {
      return(1)
    }
  }
  for (i in last_start:last_stop) {
    if (nze[i] != nze[j])
      return(0)
    j = j + 1
  }
  return(1)
}

compute_same <- function(xv, start, stop, nze, n_state_terms = 0, tol = 1e-06) {
  n_rows <- length(start)
  same <- integer(n_rows)
  # mark all `same` as 0 if n_state_terms > 0, conservatively
  if (n_state_terms > 0) {
    return(same)
  }
  same[1] <- 0L
  for (j in 2:n_rows) {
    if ((stop[j] - start[j]) == (stop[j - 1] - start[j - 1])
        && is_same_int(nze, start[j - 1], stop[j - 1], start[j], stop[j])
        && is_same_float(xv, nze, start[j - 1], stop[j - 1], start[j], stop[j], tol = tol)) {
      same[j] <- 1L
    } else {
      same[j] <- 0L
    }
  }
  same
}


