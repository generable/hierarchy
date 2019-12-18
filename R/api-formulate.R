


#' Create setting for a model matrix
#'
#' Calling this function constructs the (sparse) model
#' matrices for all models and combines the data
#' into a single list with all necessary prefixes.  
#'
#' @param models, a list of formulas.  One formula per
#"        model matrix to create.
#' @param data data.frame to pull formula terms from.
#' @return list containing 1) the list-format model
#'         matrix for each formula; 2) the data used 
#'         to evaluate terms for the matrix model; 
#'         3) the sparse-format model matrix in a
#'         container object; 4) additional config data used;
#'         and 4) the list of models supplied
#'         
#' @export
formulate = function(
  models = list(),
  data,
  configuration = list(), 
  auxilliary_data = list()
) {

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
    "col_start", "col_stop", "col_skips", "col_nze", "col_n_nze",
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
    auxilliary_data = auxilliary_data
  ))
}



