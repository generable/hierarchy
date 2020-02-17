#' Constructs a list containing the model matrix for the formulas,
#' data, and configurations provided.
#' 
#' \code{formulate} constructs the model matrix as a list-format
#' model matrix per formula, the data used to evaluate the
#' terms for the model matrix, the model matrix as a sparse
#' matrix in a container object, additioanl configuration data
#' used, and the list of models supplied.
#'
#' @param models a list of \code{formula}s. Each formula creates its
#'    own model matrix.
#' @param data \code{data.frame} to pull formula terms from.
#' @param configuration configuration
#' @param auxiliary_data auxiliary data
#' @return List representing the model matrix constructed from
#'    the parameters. The named list contains:
#'    1. \code{models}: the list of models supplied
#'    2. \code{data}: the data used to evaluate the terms of the model matrix
#'    3. \code{configuration}: any additional configuration data used
#'    4. \code{inputs}: the sparse-format model matrix as stan-formatted input
#'    5. \code{matrices}: the list-format model matrix for each formula
#'    6. \code{auxiliary_data}: any auxiliary data used
#'         
#' @export
formulate = function(models = list(), 
                     data, 
                     configuration = list(), 
                     auxiliary_data = list()) {
  checkmate::assert_data_frame(data)
  checkmate::assert_list(models, types = "formula")
  checkmate::assert_list(configuration)
  checkmate::assert_list(auxiliary_data)
  
  response_names = as.character(sapply(models, hierarchy:::lhs))
  checkmate::assert_vector(response_names, unique = TRUE, 
                           .var.name = "models (lhs)")
  if (length(response_names) > 0)
    checkmate::assert_data_frame(data, min.rows = 1)
  
  names(models) = response_names
  data = hierarchy:::setup_data(data, response_names)
  configuration = hierarchy:::setup_configuration(configuration, response_names)
  
  mm_objs = list()
  for (name in response_names) {
    mm_objs[[name]] = hierarchy:::fmm_factory(
      formula = models[[name]], 
      data = data,
      configuration = configuration[[name]])
  }

  # These are all the parts we need to pull out from the model matrix
  expose_components = c(
    "n_nze", "n_col", "n_row", 
    "start", "stop", "nze", "skips",
    "col_start", "col_stop", "col_skips", "col_nze", "col_xv_nze", "col_n_nze", "xv",
    "constant_terms", "n_constant_terms", "constant_start", "constant_stop", "constant_columns", "n_constant_columns",
    "random_terms", "n_random_terms", "random_start", "random_stop", "random_columns", "n_random_columns",
    "state_terms", "n_state_terms", "state_start", "state_stop", "state_columns", "n_state_columns",
    "coefficient_terms", "n_coefficient_terms", "coefficient_start", "coefficient_stop", "coefficient_columns", "n_coefficient_columns",
    "same")

  stan_inputs = list()
  for (name in response_names) {
    names(expose_components) = paste(name, expose_components, sep='_')
    submodel_inputs = do.call(mm_objs[[name]]$expose, as.list(expose_components))
    stan_inputs[[name]] =  submodel_inputs
  }

  return(list(
    models = models,
    data = data, 
    configuration = configuration, 
    inputs = stan_inputs, 
    matrices = mm_objs, 
    auxiliary_data = auxiliary_data
  ))
}

#' Setup data for \code{formulate}.
#' 
#' For each name in \code{response_names}, it will either
#' find a column with that name or add a new column filled
#' with 1s.
#'
#' @param data \code{data.frame} with at least one row of data
#' @param response_names \code{vector} of characters that need
#'    to be found in the data frame.
#'
#' @return \code{data.frame} where each \code{response_name} is
#'    found as a column
setup_data = function(data, response_names) {
  for (name in response_names) {
    if (!(name %in% names(data)))
      data[[name]] = 1
  }
  return(data)
}

#' Setup configuration list for \code{formulate}.
#' 
#' For each name in \code{response_names}, it will either
#' find the configuration inside the list or it will put
#' an empty list there.
#'
#' @param configuration named \code{list}
#' @param response_names \code{vector} of characters that need
#'    to be found in the data frame.
#'
#' @return named \code{list} where each response_name either
#'    contains a configuration that's not NULL or an empty list.
setup_configuration = function(configuration, response_names) {
  for (name in response_names) {
    if (is.null(configuration[[name]]))
      configuration[[name]] = list()
  }
  return(configuration)  
}
