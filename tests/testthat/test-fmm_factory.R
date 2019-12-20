library(hierarchy)

context("Testing fmm_factory: flat model matrix refclass")

test_that('fmm_factory creates the model matrix properly', {
  formula = Y ~ 1
  data = data.frame(Y = 1)
  configuration = list()
  
  
  expect_success(hierarchy::fmm_factory(formula, data, configuration))
})


setup_1 = function() {
  formula = Y ~ 1
  data = data.frame(Y = 1)
  configuration = list()

  return(hierarchy::fmm_factory(formula, data, configuration))
}

test_that('fmm_factory fields are well-formed', {
  fmm = setup_1()
  
})