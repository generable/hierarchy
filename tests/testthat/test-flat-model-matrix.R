library(testthat)


test_that("simple intercept model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1
  data = data.frame(X = 2, treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(mm[,1], rep(1, nrow(mm)))
})

test_that("simple intercept + contrast model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1 + treatment_type
  data = data.frame(X = 2, treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(mm[,1], rep(1, nrow(mm)))
  expect_equivalent(Matrix::colSums(mm[,2:26]), rep(1, 25))
})

test_that("simple no-intercept + contrast model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 0 + treatment_type
  data = data.frame(X = 2, treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), rep(1, 25))
})

# FIXME: this is not intuitive but maybe not fixable
#        given the desire for consistency.  A bare name
#        is always a contrast (non of this conditional
#        stuff).
test_that("simple contrast model is correctly constructed", {
  library(hierarchy)
  formula = X ~ treatment_type
  data = data.frame(X = 2, treatment_type =c(letters, letters))
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), rep(2, 25))
})

test_that("simple factor-intercept model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept(treatment_type)
  data = data.frame(X = 2, treatment_type =c(letters, letters))
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), rep(2, 26))
})


test_that("simple spline model is correctly constructed", {
  library(hierarchy)
  formula = X ~ radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100))
  configuration = list(k = 8)
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(ncol(mm), configuration$k)
  expect_equivalent(Matrix::rowSums(mm), rep(1, nrow(mm)))
})



