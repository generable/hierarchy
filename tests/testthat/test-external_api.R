library(testthat)

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

test_that('formulate checks input for correct types', {
  expect_error(hierarchy::formulate(models = X ~ 1 + treatment_type, data = data.frame()),
               "Must be of type 'list'.*")
  expect_error(hierarchy::formulate(models = 123, data = data.frame()),
               "Must be of type 'list'.*")
               
})

test_that('formulate checks data for size', {
  expect_error(hierarchy::formulate(models = list(Y ~ X), 
                                    data = data.frame()),
               ".*Must have at least 1 rows.*")
})

test_that('formulate checks that models do not duplicate lhs', {
  expect_error(hierarchy::formulate(models = list(Y ~ 1, Y ~ X),
                                    data = data.frame()),
               ".*'models \\(lhs\\)'.*Contains duplicated values.*")
})

test_that('setup_data handles names properly', {
  df = data.frame(X = 0)
  
  data = hierarchy:::setup_data(df, c("X"))
  expect_identical(data, df)
  
  
  data = hierarchy:::setup_data(df, c("Y"))
  expect_identical(data, data.frame(X = 0, Y = 1))
  
  data = hierarchy:::setup_data(df, c("X", "Y"))
  expect_identical(data, data.frame(X = 0, Y = 1))
  
  data = hierarchy:::setup_data(df, c("X", "Z", "Y"))
  expect_identical(data, data.frame(X = 0, Z = 1, Y = 1))
})

test_that('setup_configuration handles names properly', {
  config = list(X = list())
  configuration = hierarchy:::setup_configuration(config, c("X"))
  expect_identical(configuration, config)
  
  # test that X gets replaced
  configuration = hierarchy:::setup_configuration(list(X = NULL), c("X"))
  expect_identical(configuration, list(X = list()))

  # test that X stays the same
  configuration = hierarchy:::setup_configuration(list(X = 'foo'), c("X"))
  expect_identical(configuration, list(X = 'foo'))
  
  configuration = hierarchy:::setup_configuration(list(X = NULL), c("X", "Y"))
  expect_identical(configuration, list(X = list(), Y = list()))
})


test_that('formulate works for a simple model', {
  models = list(X ~ 1 + treatment_type)
  data = data.frame(treatment_type = letters)
  configuration = list()
  
  specification = hierarchy::formulate(models, data, configuration)
  expect_equal(specification[['data']]$treatment_type, data$treatment_type)
  mm = specification$matrices$X$.model$matrix
  expect_equal(colnames(mm), c("intercept", letters[2:26]))
  expect_equal(mm[,1], rep(1, nrow(mm)))
  expect_equivalent(colSums(as.matrix(mm)), c(nrow(mm), rep(1, 25)))
})
