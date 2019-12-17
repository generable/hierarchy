library(hierarchy)

context("Testing the external API")

test_that('formulate accepts empty objects', {
  specification = hierarchy::formulate()
})

test_that('formulate works for a simple model', {
  models = list(X ~ 1 + treatment_type)
  data = data.frame()
  configuration = list()
  
  specification = hierarchy::formulate(models, data, configuration)
})