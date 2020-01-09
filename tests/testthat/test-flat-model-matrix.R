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
  expect_equivalent(Matrix::rowSums(mm), rep(1, 2 * 26))
})

test_that("simple factor-random model is correctly constructed", {
  library(hierarchy)
  formula = X ~ random(treatment_type)
  data = data.frame(X = 2, treatment_type =c(letters, letters))
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), rep(2, 26))
  expect_equivalent(Matrix::rowSums(mm), rep(1, 2 * 26))
  expect_equivalent(fmm$.re_start, 1)
  expect_equivalent(fmm$.re_stop, 26)
  expect_equal(fmm$.re_names, 'random(treatment_type)')
})

test_that("simple intercept plus factor-random model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept() + random(treatment_type)
  data = data.frame(X = 2, treatment_type =c(letters, letters))
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), c(nrow(mm), rep(2, 26)))
  expect_equivalent(Matrix::rowSums(mm), rep(2, 2 * 26))
  expect_equivalent(fmm$.re_start, 2)
  expect_equivalent(fmm$.re_stop, 27)
  expect_equal(fmm$.re_names, c('random(treatment_type)'))
})

test_that("simple intercept plus factor-random model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept() + random(treatment_type)
  data = data.frame(X = 2, treatment_type =c(letters, letters))
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equivalent(Matrix::colSums(mm), c(nrow(mm), rep(2, 26)))
  expect_equivalent(Matrix::rowSums(mm), rep(2, 2 * 26))
  expect_equivalent(fmm$.re_start, 2)
  expect_equivalent(fmm$.re_stop, 27)
  expect_equal(fmm$.re_names, c('random(treatment_type)'))
})

test_that("simple intercept plus factor-random interaction model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept() + dogs:random(treatment_type)
  data = data.frame(X = 2, 
    treatment_type =c(letters[1:21], letters[1:21], letters[1:21]),
    dogs = toupper(letters[1:3])
  )
  used = data %>% dplyr::group_by(dogs, treatment_type) %>%
    filter(dogs != "A") %>%
    summarize(count = n()) %>%
    ungroup() %>% 
    transmute(cn = paste(dogs, treatment_type, sep = '::'),
              count = count)
  used_columns = used %>% select(cn) %>% unlist(use.names=FALSE)
  used_counts = used %>% select(count) %>% unlist(use.names=FALSE)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  used_columns = which(colnames(mm) %in% used_columns)
  expect_equivalent(Matrix::colSums(mm[,used_columns]), used_counts)  
  expect_equivalent(Matrix::rowSums(mm), rep(1, nrow(mm)) + (data$dogs != "A"))
  expect_equivalent(fmm$.re_start, 2)
  expect_equivalent(fmm$.re_stop, ncol(mm))
  expect_equal(fmm$.re_names, c('dogs, random(treatment_type)'))
})

test_that("simple intercept plus factor-random interaction model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept() + intercept(dogs):random(treatment_type)
  data = data.frame(X = 2, 
    treatment_type =c(letters[1:21], letters[1:21], letters[1:21]),
    dogs = toupper(letters[1:3])
  )
  used = data %>% dplyr::group_by(dogs, treatment_type) %>%
    filter(dogs != "A") %>%
    summarize(count = n()) %>%
    ungroup() %>% 
    transmute(cn = paste(dogs, treatment_type, sep = '::'),
              count = count)
  used_columns = used %>% select(cn) %>% unlist(use.names=FALSE)
  used_counts = used %>% select(count) %>% unlist(use.names=FALSE)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  used_columns = which(colnames(mm) %in% used_columns)
  expect_equivalent(Matrix::colSums(mm[,used_columns]), used_counts)  
  expect_equivalent(Matrix::rowSums(mm), rep(2, nrow(mm)))
  expect_equivalent(fmm$.re_start, 2)
  expect_equivalent(fmm$.re_stop, ncol(mm))
  expect_equal(fmm$.re_names, c('intercept(dogs), random(treatment_type)'))
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

test_that("simple spline model with four knots is correctly constructed", {
  library(hierarchy)
  formula = X ~ radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100))
  configuration = list(k = 4)
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(ncol(mm), configuration$k)
  expect_equivalent(Matrix::rowSums(mm), rep(1, nrow(mm)))
})

test_that("simple spline model with three knots fails.", {
  library(hierarchy)
  formula = X ~ radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100))
  configuration = list(k = 4)
  expect_error(fmm = fmm_factory(formula, data, configuration))
})

test_that("simple spline-factor interaction model is correctly constructed", {
  library(hierarchy)
  formula = X ~ intercept(dogs):radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100), 
    dogs = sample(letters[1:4], 100, replace = TRUE))
  configuration = list(k = 8)
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(ncol(mm), length(unique(data$dogs)) * configuration$k)
  expect_equivalent(Matrix::rowSums(mm), rep(1, nrow(mm)))
})

test_that("simple spline-random-factor interaction model is correctly constructed", {
  library(hierarchy)
  formula = X ~ random(dogs):radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100), 
    dogs = sample(letters[1:4], 100, replace = TRUE))
  configuration = list(k = 8)
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(ncol(mm), length(unique(data$dogs)) * configuration$k)
  expect_equivalent(Matrix::rowSums(mm), rep(1, nrow(mm)))
  expect_equal(fmm$.re_names, fmm$.term_names)
  expect_equivalent(fmm$.re_start, 1)
  expect_equivalent(fmm$.re_stop, 32)
})

test_that("simple intercept plus spline-contrast interaction model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1 + dogs:radial_b_spline(z, k, min, max)
  data = data.frame(X = 2, z = rnorm(100), 
    dogs = sample(letters[1:4], 100, replace = TRUE))
  configuration = list(k = 8)
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(ncol(mm), fmm$.term_stop[2])
  row_sums = rep(1, nrow(mm))
  row_sums[data$dogs != 'a'] = 2
  expect_equivalent(Matrix::rowSums(mm), row_sums)
})

test_that("simple intercept + covariate model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1 + covariate(zerg)
  data = data.frame(X = 2, zerg = rnorm(26), treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(colnames(mm), c('intercept', 'zerg'))
  expect_equal(mm[,'intercept'], rep(1, nrow(mm)))
  expect_equal(mm[,'zerg'], data$zerg)
})

test_that("simple intercept + state model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1 + state(zerg)
  data = data.frame(X = 2, bomo = rnorm(26), treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(colnames(mm), c('intercept', 'zerg'))
  expect_equal(mm[,'intercept'], rep(1, nrow(mm)))
  expect_equal(mm[,'zerg'], rep(1, nrow(mm)))
  expect_equal(fmm$.term_names, c('intercept()', 'state(zerg)'))
  expect_equivalent(fmm$.term_start, 1:2)
  expect_equivalent(fmm$.term_stop,1:2)
})

test_that("simple intercept + bare covariate model is correctly constructed", {
  library(hierarchy)
  formula = X ~ 1 + zerg
  data = data.frame(X = 2, zerg = rnorm(26), treatment_type = letters)
  configuration = list()
  fmm = fmm_factory(formula, data, configuration)
  mm = fmm$.model$matrix
  expect_equal(colnames(mm), c('intercept', 'zerg'))
  expect_equal(mm[,'intercept'], rep(1, nrow(mm)))
  expect_equal(mm[,'zerg'], data$zerg)
})
