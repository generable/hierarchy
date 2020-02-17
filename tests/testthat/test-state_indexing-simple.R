
library(testthat)
library(tidyverse)
testthat::context('test state_terms vs state_columns')
data(gtcars, package = 'gt')


test_that('.state_terms & .state_columns are right in the simplest case', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  mm <- formulate(models = list(X ~ ctry_origin + state(a)), data = gtcars)
  # state term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.state_terms, 2)
  # state_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.state_columns, num_dummies + 1)
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, 1)
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + 1)
  
  # test stan `inputs` list
  inputs <- mm$inputs$X
  # double-check `state` items
  testthat::expect_equivalent(inputs$X_state_terms, mm$matrices$X$.state_terms)
  testthat::expect_equivalent(inputs$X_state_columns, mm$matrices$X$.state_columns)
  testthat::expect_equivalent(inputs$X_state_start, min(mm$matrices$X$.state_columns))
  testthat::expect_equivalent(inputs$X_state_stop, max(mm$matrices$X$.state_columns))
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(inputs$X_coefficient_start, min(mm$matrices$X$.coefficient_start))
  testthat::expect_equivalent(inputs$X_coefficient_stop, max(mm$matrices$X$.coefficient_stop))
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + 1)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.state_terms & .state_columns are right in the simplest case, order reversed', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  mm <- formulate(models = list(X ~ state(a) + ctry_origin), data = gtcars)
  # state term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.state_terms, 1)
  # state_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.state_columns, 1)
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, 1 + seq_len(num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, 1 + 1)
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + 1)
  
  inputs <- mm$inputs$X
  # double-check `state` items
  testthat::expect_equivalent(inputs$X_state_terms, mm$matrices$X$.state_terms)
  testthat::expect_equivalent(inputs$X_state_columns, mm$matrices$X$.state_columns)
  testthat::expect_equivalent(inputs$X_state_start, min(mm$matrices$X$.state_columns))
  testthat::expect_equivalent(inputs$X_state_stop, max(mm$matrices$X$.state_columns))
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(inputs$X_coefficient_start, min(mm$matrices$X$.coefficient_start))
  testthat::expect_equivalent(inputs$X_coefficient_stop, max(mm$matrices$X$.coefficient_stop))
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + 1)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})


test_that('.state_terms & .state_columns with 2 state_terms, standard order', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_states <- 2
  mm <- formulate(models = list(X ~ ctry_origin + state(a) + state(b)), data = gtcars)
  # state term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.state_terms, 1 + seq_len(num_states))
  # state_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.state_columns, num_dummies + seq_len(num_states))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, 1)
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_states)
  
  inputs <- mm$inputs$X
  # double-check `state` items
  testthat::expect_equivalent(inputs$X_state_terms, mm$matrices$X$.state_terms)
  testthat::expect_equivalent(inputs$X_state_columns, mm$matrices$X$.state_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(inputs$X_coefficient_start, min(mm$matrices$X$.coefficient_start))
  testthat::expect_equivalent(inputs$X_coefficient_stop, max(mm$matrices$X$.coefficient_stop))
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_states)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.state_terms & .state_columns are right with an interaction, order reversed', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_state_terms <- 2
  num_state_columns <- 1 + num_dummies
  mm <- formulate(models = list(X ~ ctry_origin + state(a) + ctry_origin:state(a)), data = gtcars)
  # state term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.state_terms, 1 + seq_len(num_state_terms))
  # state_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.state_columns, num_dummies + seq_len(1 + num_dummies))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, 1)
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_state_columns)
  
  inputs <- mm$inputs$X
  # double-check `state` items
  testthat::expect_equivalent(inputs$X_state_terms, mm$matrices$X$.state_terms)
  testthat::expect_equivalent(inputs$X_state_columns, mm$matrices$X$.state_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_state_start, inputs$X_state_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.state_columns)
  
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(inputs$X_coefficient_start, min(mm$matrices$X$.coefficient_start))
  testthat::expect_equivalent(inputs$X_coefficient_stop, max(mm$matrices$X$.coefficient_stop))
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_state_columns)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.state_terms & .state_columns are right with an intercept(), order reversed', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin)
  num_states <- 1
  mm <- formulate(models = list(X ~ intercept(ctry_origin) + state(a)), data = gtcars)
  # state term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.state_terms, 1 + seq_len(num_states))
  # state_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.state_columns, num_dummies + seq_len(num_states))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, 1)
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_states)
  testthat::expect_equivalent(mm$matrices$X$get_coefficient_names(), dimnames(mm$matrices$X$get_matrix())[[2]])
})
