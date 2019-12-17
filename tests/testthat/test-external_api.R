library(hierarchy)

context("Testing the external API")

test_that('formulate handles missing data object', {
  expect_error(hierarchy::formulate(), 
               'argument "data" is missing.*')
})

test_that('formulate handles empty data object', {
  specification = hierarchy::formulate(data = data.frame())
  checkmate::expect_list(specification, names = 'named')
  
  checkmate::expect_list(specification$models)
  expect_length(specification$models, 0)
  
  checkmate::expect_data_frame(specification$data)
  expect_length(specification$data, 0)
  
  checkmate::expect_list(specification$input)
  expect_length(specification$input, 0)
  
  checkmate::expect_list(specification$configuration)
  expect_length(specification$configuration, 0)
  
  checkmate::expect_list(specification$matrices)
  expect_length(specification$matrices, 0)
  
  checkmate::expect_list(specification$auxiliary_data)
  expect_length(specification$auxiliary_data, 0)
})


test_that('formulate works for a simple model', {
  skip('')
  models = list(X ~ 1 + treatment_type)
  data = data.frame()
  configuration = list()
  
  specification = hierarchy::formulate(models, data, configuration)
})