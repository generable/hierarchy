#' A class for creating a flat sparse model matrix
#' representation
#'
#' @field .n_row number of observations in model matrix
#' @field .n_col number of columns in the model matrix
#' @field .n_nze number of non-zero entries in the model matrix
#' @field .nze indexes of non-zero entries in the model matrix (RSC)
#' @field .start where each row of the model matrix start in nze
#' @field .stop where each row of the model matrix ends in nze
#' @field .xv n_nze nze entries of the model matrix 
#' @field .y dependent data vector, if applicable
#' @field .groups index into xv for each group of parameters produced by the formula
#' @export fmm_factory
#' @exportClass fmm
fmm_factory = methods::setRefClass(Class = "fmm",
  fields = list(
    .specifiers = "list",
    .components = "list",
    .blocks = "list",
    .model = "list",
    .matrix = "list",
    .n_row = "numeric",
    .n_col = "numeric",
    .n_nze = "numeric",
    .nze = "array",
    .skips = "array",
    .start = "array",
    .stop = "array",
    .col_n_nze = "numeric",
    .col_skips = "array",
    .col_start = "array",
    .col_stop = "array",
    .col_nze = "array",
    .col_xv_nze = "array",
    .xv = "numeric",
    .y_name = "character",
    .y = "list",
    .term_width = "array",
    .n_terms = "numeric",
    .term_start = "array",
    .term_stop = "array",
    .term_names = "character",
    .group_columns = "list",
    .group_terms = "list",
    .col_group = "character",
    .col_terms = "list",
    .col_names = "character",
    .group_lengths = "list",
    .state_terms = "array",
    .n_state_terms = "numeric",
    .constant_terms = "array",
    .n_constant_terms = "numeric",
    .coefficient_terms = "array",
    .n_coefficient_terms = "numeric",
    .random_terms = "array",
    .n_random_terms = "numeric",
    .re_names = "character",
    .n_re = "numeric",
    .n_re_effects = "numeric",
    .re_start = "array",
    .re_stop = "array",
    .state_names = "character",
    .state_start = "array",
    .state_stop = "array",
    .data = "environment",
    .configuration = "environment"
  ),
  methods = list(
    initialize = function(formula, data, configuration, N = nrow(data), ...) {
      "Create the implicit mass matrix and store components."
      .self$.specifiers = list(
        original = formula,
        simple = simplify(formula),
        standard = distribute(simplify(formula))
      )
      .self$.specifiers[['term']] = subterms(.self$.specifiers$standard)
      .self$.specifiers[['term_list']] = term_list(.self$.specifiers$term) 
      if (!is.list(.self$.specifiers[['term_list']]))
        .self$.specifiers[['term_list']] = list(.self$.specifiers$term_list)
      .self$.data = as.environment(data)
      .self$.configuration = as.environment(configuration)
      .self$.data$N = N
      .self$.components = imbue(
        terms = .self$.specifiers$term_list$rhs, 
        data = .self$.data, 
        configuration = .self$.configuration)

      .self$.blocks = list(subterm = expand(.self$.components))
      .self$.blocks[['term']] = combine_subterms(.self$.blocks$subterm)
      .self$.model = list(
        matrix = combine_terms(.self$.blocks$term),
	      list = m_as_list(combine_terms(.self$.blocks$term))
      )
      .self$.n_row = nrow(.model$matrix)
      .self$.n_col = ncol(.model$matrix)
      .self$.n_nze = .model$list$n_nze
      .self$.nze = array(.model$list$nze)
      .self$.skips = array(.model$list$skip)
      .self$.start = array(.model$list$start)
      .self$.stop = array(.model$list$stop)
      .self$.xv = .model$list$xv
      .self$.col_n_nze = .model$list$col_n_nze
      .self$.col_nze = array(.model$list$col_nze)
      .self$.col_xv_nze = array(.model$list$col_xv_nze)
      .self$.col_skips = array(.model$list$col_skip)
      .self$.col_start = array(.model$list$col_start)
      .self$.col_stop = array(.model$list$col_stop)
      .self$.col_stop = array(.model$list$col_stop)

      # Response
      .self$.y_name = deparse(.self$.specifiers$term_list['lhs'])
      .self$.y = .self$.components = imbue(.self$.specifiers$term_list['lhs'], .self$.data)

      # Column grouping
      .self$.term_width = array(sapply(.self$.blocks$term, ncol))
      .self$.n_terms = length(.self$.term_width)
      .self$.term_stop = array(cumsum(.self$.term_width))
      .self$.term_start = array(c(1, .self$.term_stop[-.self$.n_terms] + 1))
      .self$.term_names = sapply(.self$.specifiers$term_list[['rhs']], deparse)

      # Group to columns    FIXME: need this
      #.self$.group_columns = mml$group_columns
      #.self$.group_terms = mml$group_terms

      # Columns to (multiple) groups   FIXME: need this
      #.self$.col_group = mml$col_group
      #.self$.col_terms = mml$col_terms
      #.self$.col_names = mml$names
      #.self$.group_lengths = mml$group_lengths
      
      # effect types
      term_block_expressions = names(.self$.blocks$term)
      .self$.state_terms = array(which(sapply(parse(text = term_block_expressions), has_, 'state')))
      .self$.n_state_terms = length(.self$.state_terms)
      .self$.constant_terms = array(which(sapply(parse(text = term_block_expressions), has_, 'constant')))
      .self$.n_constant_terms = length(.self$.constant_terms)
      .self$.coefficient_terms = array(setdiff(1:.self$.n_col,
        c(.self$.state_terms, .self$.constant_terms)))
      .self$.n_coefficient_terms = length(.self$.coefficient_terms)
      .self$.random_terms =  array(which(sapply(parse(text = term_block_expressions), has_, 'random')))
      .self$.n_random_terms = length(.self$.random_terms)

      # r.e. indexing.
      .self$.re_names = .self$.term_names[.self$.random_terms]
      .self$.n_re = length(.self$.random_terms)
      .self$.re_start = array(.self$.term_start[.self$.random_terms])
      .self$.re_stop = array(.self$.term_stop[.self$.random_terms])

      # state indexing.
      .self$.state_names = .self$.term_names[.self$.state_terms]
      .self$.state_start = array(.self$.term_start[.self$.state_terms])
      .self$.state_stop = array(.self$.term_stop[.self$.state_terms])
    },
    expose = function(...) {
      "Extractor that takes a named vector and provides the relevant
       component with (optionally) a new name.  The renaming syntax follows
       dplyr::rename so the new name is taken from the name of the argument and
       the element to extract is taken from the character vector content.

       For example OBJ$expose(phi_n_col = 'n_col') would return the number of (implicit)
       model matrix columns with the name 'phi_n_col'.  This is useful to construct
       lists that are going to be used in, e.g.-Stan."
      fields = names(methods::getRefClass("fmm")$fields())
      args_internal = unlist(list(...))
      args_new = names(args_internal)
      o = list()
      for (i in 1:length(args_internal)) {
        arg = args_internal[i]
        internal_arg = paste0(".", arg)
        if (internal_arg %in% fields) {
          if (length(args_new) != 0 && args_new[i] != "")
            o[[args_new[i]]] = .self[[internal_arg]]
          else
            o[[arg]] = .self[[internal_arg]] 
        }
      }
      return(o)
    },
    list_components = function() {
      "Get a list of the object's fields that can be exposed."
      fields = names(methods::getRefClass("fmm")$fields())
      return(fields)
    },
    list_terms = function() {
      return(names(.self$.group_terms))
    },
    get_data = function() {
      "Get the data frame used to construct the matrix."
      return(as.list(.self$.data))
    },
    n_row = function() .self$.n_row,
    n_col = function() .self$.n_col,
    check_component = function(component) {
      "Verify that the requested (formula) component is 
       in the model matrix and return its name.  If none
       is specified (NULL in calling function) then all 
       are returned."
      available_components = .self$list_terms()
      if (is.null(component)) {
        return(available_components) 
      } else {
        bad = component[!(component %in% available_components)]
        if (length(bad) != 0) {
          msg = paste0("Some components are not involved in this ",
                       "model matrix.  Extraneous components are: \n",
                       paste("    ", bad, sep = "", collapse = "\n"), 
                       "\nAvailable components are: \n",
                       paste("    ", available_components, sep = "", collapse = " \n"))
          stop(msg)
        }
        return(component)
      }
    }, 
    n_nze = function() .self$.n_nze,
    nze = function() .self$.nze,
    skips = function() .self$.skips,
    start = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.start[component])
    },
    stop = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.stop[component])
    },
    x = function(component = NULL) {
      component = .self$check_component(component)
      groups = .self$group(component)
      start = .self$.start
      stop = .self$.stop
      o = list()
      for (i in 1:length(component))
        o[[c]] = .self$.xv[start[i]:stop[i]]
      return(o)
    },
    groups = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.groups[component])
    },
    get_matrix = function(flat=FALSE) {
      if (flat)
        return(.sefl$.model$list)
      else
        return(.self$.model$matrix)
    }
  )
)
