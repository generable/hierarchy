

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

row_lengths_equiv <- function(start, stop) {
  (start - stop == dplyr::lag(start) - dplyr::lag(stop)) %>%
    as.logical() %>%
    purrr::modify_if(~ is.na(.), ~ FALSE)
}

#' construct list of values for each row, given start & stop indices
#' (where NA indices result in NA values & 0-valued indices result in 0 values)
row_part <- function(vals, start, stop) {
  purrr::map2(start, stop, 
              ~ if (is.na(.x)) {NA} else if (.x == 0) {0} else {vals[.x:.y]})
}

test_near <- function(a, b, ...) {
  if (length(a) == length(b)) {
    dplyr::near(a, b, ...)
  } else {
    # if lengths differ, all comparisons are FALSE
    purrr::rep_along(a, FALSE)
  }
}
#' test each row of sparse-matrix values against a previous row's values
#' where each row's values are indexed by start & stop vectors
#' 
#' @return a logical vector of length start
row_parts_equal_prev <- function(vals, start, stop) {
  # test each row against its previous values
  purrr::map2(row_part(vals, start, stop),
              row_part(vals, dplyr::lag(start), dplyr::lag(stop)),
              test_near) %>%
    # convert 0-length vector to FALSE
    purrr::modify_if(~ length(.) == 0, ~ FALSE) %>%
    # a row is TRUE only if all values are equivalent
    purrr::map_lgl(all) %>%
    # NA values (the first row) are FALSE
    purrr::modify_if(~ is.na(.), ~ FALSE)
}

#' compute the `_same` vector indicating which rows are equivalent to the previous row
#' in a sparse matrix context.
#' @return an integer vector of length start where 0: not the same and 1: same
compute_same <- function(xv, start, stop, nze, n_state_terms = 0) {
  n_rows <- length(start)
  same <- integer(n_rows)
  # mark all `same` as 0 if n_state_terms > 0, conservatively
  if (n_state_terms > 0) {
    return(same)
  }
  row_len_equiv <- row_lengths_equiv(start, stop)
  row_nze_equiv <- row_parts_equal_prev(nze, start, stop)
  row_xv_equiv <- row_parts_equal_prev(xv, start, stop)
  same <- dplyr::if_else(row_len_equiv & row_nze_equiv & row_xv_equiv,
                         1L, 0L)
}


index_terms = function(x, type) {
  o = grepl(paste0(type, '\\('), names(x$.blocks$term)) %>% 
      which() %>% array
  return(o)
}

merge_start_stop = function(start, stop) {
  stopifnot(isTRUE(all(unlist(purrr::map2(start, stop, ~ .x <= .y)))))
  o = purrr::map2(start, stop, ~ .x:.y) %>% 
    purrr::flatten() %>% 
    purrr::map_int(~.) %>% 
    array()
  return(o)
}

#' Combine start/stops into a vector of indexes
block_by_type = function(x, type, terms = NULL) {
  if (is.null(terms)) {
    terms = index_terms(x, type)
  }
  names = x$.term_names[terms]
  if (length(names) > 0) {
    start = x$.term_start[terms]
    stop = x$.term_stop[terms]
    columns = merge_start_stop(start, stop)
  } else {
    start = integer()
    stop = integer()
    columns = integer()
  }
  o = list(terms = array(terms), names = names,
    start = array(start), stop = array(stop), columns = array(columns),
    n_terms = length(terms), n_columns = length(columns))
  return(o)
}
  
