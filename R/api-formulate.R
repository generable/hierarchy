#' Constructs a list containing the model matrix for the formulas,
#' data, and configurations provided.
#' 
#' \code{formulate} constructs the model matrix as a list-format
#' model matrix per formula, the data used to evaluate the
#' terms for the model matrix, the model matrix as a sparse
#' matrix in a container object, additioanl configuration data
#' used, and the list of models supplied.
#'
#' @param models a list of formulas. Each formula creates its
#'    own model matrix.
#' @param data data.frame to pull formula terms from.
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
  checkmate::qassert(data, 'd')
  
  response_names = as.character(sapply(models, hierarchy:::response_name))
  for (name in response_names) {
    if (name %in% names(data))
      next
    else
      data[[name]] = 1
  }
  names(models) = response_names

  mm_objs = list()
  for (name in response_names) {
    if (is.null(configuration[[name]]))
      configuration[[name]] = list()
    mm_objs[[name]] = hierarchy:::fmm_factory(
      formula = models[[name]], data = data,
      configuration = configuration[[name]])
  }

  # These are all the parts we need to pull out from the model matrix
  expose_components = c(
    "n_nze", "n_col", "n_row", 
    "start", "stop", "nze", "skips",
    "col_start", "col_stop", "col_skips", "col_nze", "col_xv_nze", "col_n_nze",
    "state_terms", "n_state_terms", "state_start", "state_stop",
    "constant_terms", "n_constant_terms",
    "coefficient_terms", "n_coefficient_terms",
    "n_re", "re_start", "re_stop", "xv")

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



