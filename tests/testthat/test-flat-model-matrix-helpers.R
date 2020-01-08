library(testthat)

test_that("'st' transposes a sparse matrix", {
  library(hierarchy)
  for (i in 1:10) {
    nr = sample.int(i,1)
    nc = sample.int(i,1)
    A = generate_sparse_matrix(nr, nc, 0.5)
    A = as(A, 'Matrix')
    expect_equal(A, hierarchy:::st(hierarchy:::st(A)))
  }
})


test_that("'get_nze' collects non-zero entries.", {
  library(hierarchy)
  for (i in 1:10) {
    nr = sample.int(i,1)
    nc = sample.int(i,1)
    A = generate_sparse_matrix(nr, nc, 0.03)
    nze = get_nze(A)
    for (r in 1:nr) {
      row_values = A[r,]
      expect_equal(row_values[nze[[r]]], row_values[row_values != 0])
    }
  }
})

