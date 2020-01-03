

generate_sparse_matrix = function(nr, nc, sparsity) {
  n_entries= nr * nc
  nnz_entries = ceiling(n_entries * (1 - sparsity))
  r_idx = sample.int(nr, nnz_entries, replace = TRUE)
  c_idx = sample.int(nc, nnz_entries, replace = TRUE)
  A = matrix(data = 0, nrow = nr, ncol = nc)
  xv = rnorm(nnz_entries)
  for (i in 1:nnz_entries) {
    A[r_idx[i], c_idx[i]] = xv[i]
  }
  return(A)
}

make_coefficients = function(A, pc) {
  n_coef = ncol(A)
  n_c_terms = ceiling(n_coef * pc)
  n_s_terms = n_coef - n_c_terms
  n_re_terms = sample.int(5, 1)
  scaling = rexp(n_re_terms)
  c_terms = 1:n_c_terms
  if (n_c_terms == n_coef) {
    s_terms = vector()
  } else {
    s_terms = (n_c_terms + 1):n_coef
  }
  re_start = sort(sample.int(ceiling(n_coef/n_re_terms), n_re_terms))
  re_stop = c(re_start[-1], n_coef)
  unscaled_coef = rnorm(n_coef)
  scaled_coef = unscaled_coef
  for (i in 1:n_re_terms) {
    re_stop[i] = re_start[i] + sample.int(re_stop[i] - re_start[i], 1) - 1
    scaled_coef[re_start[i]:re_stop[i]] = scaled_coef[re_start[i]:re_stop[i]] * scaling[i]
  }
  return(list(
    n_coef = n_coef, n_c_terms = n_c_terms, n_s_terms = n_s_terms,
    c_terms = c_terms, s_terms = s_terms,
    n_re_terms = n_re_terms, scale = scaling, re_start = re_start, re_stop = re_stop,
    coefficients = unscaled_coef[c_terms],
    states = unscaled_coef[s_terms],
    scaled_coef = scaled_coef))
}
