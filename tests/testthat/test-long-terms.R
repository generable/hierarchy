
library(testthat)
library(tidyverse)
testthat::context('test long term names')
data(gtcars, package = 'gt')

test_that('one can create an fmm with a long term', {
  mm <- formulate(models = list(X ~ intercept(ctry_origin):radial_b_spline(trq_rpm, k, min, max) + 
                                  state(a)), 
                  data = gtcars %>% dplyr::filter(!is.na(ctry_origin) & !is.na(trq_rpm)),
                  configuration = list(X = list(k = 5, min = 3000, max = 9000)))
})
