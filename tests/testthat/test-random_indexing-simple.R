
library(testthat)
library(tidyverse)
testthat::context('test random_terms vs random_columns')
data(gtcars, package = 'gt')


test_that('.random_terms & .random_columns are right in the simplest case', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_random_terms <- 1
  num_random_columns <- dplyr::n_distinct(gtcars$year)
  mm <- formulate(models = list(X ~ ctry_origin + random(year)), data = gtcars)
  # random term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.random_terms, 1 + seq_len(num_random_terms))
  # random_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.random_columns, num_dummies + seq_len(num_random_columns))
  # coefficient columns should be seq_len(num_dummies)
  # interesting - random_coefficients are _included_ in the set of .coefficient columns,
  #   whereas state_columns are not .. 
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies + num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, seq_len(2))
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_random_columns)
  
  # test stan `inputs` list
  inputs <- mm$inputs$X
  # double-check `random` items
  testthat::expect_equivalent(inputs$X_random_terms, mm$matrices$X$.random_terms)
  testthat::expect_equivalent(inputs$X_random_columns, mm$matrices$X$.random_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_random_start, inputs$X_random_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.random_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_coefficient_start, inputs$X_coefficient_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.coefficient_columns)
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_random_columns)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.random_terms & .random_columns are right in the simplest case, order reversed', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_random_columns <- dplyr::n_distinct(gtcars$year)
  mm <- formulate(models = list(X ~ random(year) + ctry_origin), data = gtcars)
  # random term is the 1st term
  testthat::expect_equivalent(mm$matrices$X$.random_terms, 1)
  # random_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.random_columns, seq_len(num_random_columns))
  # coefficient columns should be seq_len(num_dummies)
  # interesting - random_coefficients are _included_ in the set of .coefficient columns,
  #   whereas state_columns are not .. 
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies + num_dummies))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, seq_len(2))
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_random_columns)
  
  # test stan `inputs` list
  inputs <- mm$inputs$X
  # double-check `random` items
  testthat::expect_equivalent(inputs$X_random_terms, mm$matrices$X$.random_terms)
  testthat::expect_equivalent(inputs$X_random_columns, mm$matrices$X$.random_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_random_start, inputs$X_random_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.random_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_coefficient_start, inputs$X_coefficient_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.coefficient_columns)
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_random_columns)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})


test_that('.random_terms & .random_columns with 2 random_terms, standard order', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_randoms <- 2
  # be explicit here to minimize risk of an error in the test
  num_random_columns1 <- dplyr::n_distinct(gtcars$year)
  num_random_columns2 <- dplyr::n_distinct(gtcars$drivetrain)
  mm <- formulate(models = list(X ~ ctry_origin + random(year) + random(drivetrain)), data = gtcars)
  # random term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.random_terms, 1 + seq_len(num_randoms))
  # random_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.random_columns, num_dummies + seq_len(num_random_columns1 + num_random_columns2))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies + num_random_columns1 + num_random_columns2))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, seq_len(3))
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_random_columns1 + num_random_columns2)
  
  inputs <- mm$inputs$X
  # double-check `random` items
  testthat::expect_equivalent(inputs$X_random_terms, mm$matrices$X$.random_terms)
  testthat::expect_equivalent(inputs$X_random_columns, mm$matrices$X$.random_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_random_start, inputs$X_random_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.random_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_coefficient_start, inputs$X_coefficient_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.coefficient_columns)
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_random_columns1 + num_random_columns2)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.random_terms & .random_columns are right with an interaction, standard reversed', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin) - 1
  num_random_terms <- 2
  num_random_columns1 <- dplyr::n_distinct(gtcars$year)
  num_random_columns2 <- num_random_columns1 * num_dummies
  mm <- formulate(models = list(X ~ ctry_origin + random(year) + ctry_origin:random(year)), data = gtcars)
  # random term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.random_terms, 1 + seq_len(num_randoms))
  # random_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.random_columns, num_dummies + seq_len(num_random_columns1 + num_random_columns2))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies + num_random_columns1 + num_random_columns2))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, seq_len(3))
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_random_columns1 + num_random_columns2)
  
  inputs <- mm$inputs$X
  # double-check `random` items
  testthat::expect_equivalent(inputs$X_random_terms, mm$matrices$X$.random_terms)
  testthat::expect_equivalent(inputs$X_random_columns, mm$matrices$X$.random_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_random_start, inputs$X_random_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.random_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_coefficient_start, inputs$X_coefficient_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.coefficient_columns)
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_random_columns1 + num_random_columns2)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

test_that('.random_terms & .random_columns are right with an interaction, using `intercept()` in both', {
  num_dummies <- dplyr::n_distinct(gtcars$ctry_origin)
  num_random_terms <- 2
  num_random_columns1 <- dplyr::n_distinct(gtcars$year)
  num_random_columns2 <- num_random_columns1 * num_dummies
  mm <- formulate(models = list(X ~ intercept(ctry_origin) + random(year) + intercept(ctry_origin):random(year)), data = gtcars)
  # random term is the 2nd term
  testthat::expect_equivalent(mm$matrices$X$.random_terms, 1 + seq_len(num_randoms))
  # random_column should be num_dummies + 1
  testthat::expect_equivalent(mm$matrices$X$.random_columns, num_dummies + seq_len(num_random_columns1 + num_random_columns2))
  # coefficient columns should be seq_len(num_dummies)
  testthat::expect_equivalent(mm$matrices$X$.coefficient_columns, seq_len(num_dummies + num_random_columns1 + num_random_columns2))
  testthat::expect_equivalent(mm$matrices$X$.coefficient_terms, seq_len(3))
  
  # test names of terms
  testthat::expect_equivalent(length(mm$matrices$X$get_coefficient_names()), num_dummies + num_random_columns1 + num_random_columns2)
  
  inputs <- mm$inputs$X
  # double-check `random` items
  testthat::expect_equivalent(inputs$X_random_terms, mm$matrices$X$.random_terms)
  testthat::expect_equivalent(inputs$X_random_columns, mm$matrices$X$.random_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_random_start, inputs$X_random_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.random_columns)
  # double-check `coefficient` items
  testthat::expect_equivalent(inputs$X_coefficient_terms, mm$matrices$X$.coefficient_terms)
  testthat::expect_equivalent(inputs$X_coefficient_columns, mm$matrices$X$.coefficient_columns)
  testthat::expect_equivalent(purrr::map2(inputs$X_coefficient_start, inputs$X_coefficient_stop, 
                                          seq) %>% unlist(),
                              mm$matrices$X$.coefficient_columns)
  # check X_n_col
  testthat::expect_equivalent(inputs$X_n_col, num_dummies + num_random_columns1 + num_random_columns2)
  # NO row in X should be longer than X_n_col
  testthat::expect_true(max(inputs$X_start - inputs$X_stop) < inputs$X_n_col)
})

