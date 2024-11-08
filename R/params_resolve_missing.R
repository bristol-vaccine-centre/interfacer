
#' Resolve missing values in function parameters and check consistency
#'
#' Uses relationships between parameters to iteratively fill in missing values.
#' It is possible to specify an inconsistent set of rules or data in which case
#' the resulting values will be picked up and an error thrown.
#'
#' @param ... either a set of relationships as a list of `x=y+z` expressions
#' @param .env the environment to check in (optional - defaults to `caller_env()`)
#' @param .eval_null The default behaviour (when this option is `TRUE`)
#'   considers missing values to be are either not given, given explicitly as
#'   `NULL` or given as a `NULL` default value. Sometimes we need to consider
#'   `NULL` values differently to missing values. If this is set to `FALSE` only
#'   strictly missing values are resolved, and explicit `NULL` values left as
#'   is.
#' @param .error a glue specification defining the error message. This can use
#'   parameters `.missing`, `.constraints`, `.present` and `.call` to construct
#'   an error message. If `NULL` a default message is provided that is generally
#'   sufficient.
#'
#' @concept parameter_checks
#'
#' @return nothing. Alters the `.env` environment to fill in missing values or 
#'  throws an informative error
#' @export
#'
#' @examples
#' # missing variables left with no default value in function definition
#' testfn = function(pos, neg, n) {
#'   resolve_missing(pos=n-neg, neg=n-pos, n=pos+neg)
#'   return(tibble::tibble(pos=pos,neg=neg,n=n))
#' }
#' 
#' testfn(pos=1:4, neg = 4:1)
#' testfn(neg=1:4, n = 10:7)
#' 
#' try(testfn())
#' 
#' # not enough info to infer the missing variables
#' try(testfn(neg=1:4))
#' 
#' # the parameters given are inconsistent with the relationships defined.
#' try(testfn(pos=2, neg=1, n=4))
resolve_missing = function(
    ..., 
    .env = rlang::caller_env(), 
    .eval_null = TRUE,
    .error = NULL
  ) {
  
  if (is.null(.error)) .error="unable to infer missing variable(s): {.missing} using:\n{.constraints}\ngiven known variable(s): {.present} in {.call}"
  allexprs = rlang::enexprs(...)
  
  # tmp = tryCatch(eval(exprs[[1]],envir = .env), error = function(e) NULL)
  # if (!is.null(tmp) && is.data.frame(tmp)) {
  #   env = tmp
  #   df = TRUE
  #   exprs = head(exprs,-1)
  # } else {
  env = .env
  #   df = FALSE
  # }
  df = is.data.frame(.env)
  
  # all expressions that can be used for resolution must be named. 
  # If the user passes an unnamed rule (e.g. x > 30)
  # it is used to check consistency but not for resolution.
  # it is dropped here but pucked up later
  exprs = allexprs[names(allexprs) != ""]
  
  inputs = unique(unlist(lapply(exprs, all.vars)))
  all_missing = all(.is_missing(inputs, env, or_null = TRUE))
  if (all_missing) stop("unable to infer missing variables: no non-null parameters provided", call.=FALSE)
  
  for (k in seq_along(exprs)) {
    deferred = FALSE
    for (i in seq_along(exprs)) {
      # focus is the variable that the current rule can infer
      focus = names(exprs)[[i]]
      
      if (.is_missing(focus, env, or_null = .eval_null)) {
        # The variables that the rule uses to infer
        params = all.vars(exprs[[i]])
        
        # Try and evaluate the missing value `focus` using the `params` from
        # the current rule. This can only happen if all the `params` are
        # not missing and not NULL
        if (!any(.is_missing(params, env, or_null = TRUE))) {
          # if (!df) {
            env[[focus]] = eval(exprs[[i]], env)
          # } else {
          #  env = env %>% dplyr::mutate(!!exprs[[i]])
          # }
        } else {
          deferred = TRUE
        }
      }
      
    }
    if (!deferred) {
      # If everything is resolved we can check the results and exit. The
      # function changes the environment in which it was called so returns
      # nothing.  This uses all the rules that were passed including those 
      # which were predicates rather than assignments.
      check_consistent(!!!allexprs, .env=env)
      return(invisible(env))
    }
  }
  if (deferred) {
    
    # Not every missing value could be resolved. 
    still_missing = .is_missing(names(exprs), env, or_null = .eval_null)
    mentioned = unique(unname(unlist(lapply(exprs, all.vars)))) 
    isnl = .is_missing(mentioned, env, or_null = TRUE)
    .constraints = paste0("`",names(exprs)," = ",as.character(exprs),"`", collapse = "\n")
    .missing = paste0("`",names(exprs)[still_missing],"`",collapse = ", ")
    .present = paste0("`",mentioned[!isnl],"`",collapse = ", ")
    tmp = rlang::caller_call() %>% rlang::as_label()
    if (tmp == "NULL") tmp = sprintf("%s(...)", .get_fn_name(rlang::caller_fn()))
    if (is.null(tmp)) {
      .call = "<unknown>"
    } else {
      .call = paste0("`",tmp,"`")
    }
    stop(glue::glue(.error), call.=FALSE)
  }
  invisible(env)
}

# vectorised by name
.is_missing = function(name, env, or_null = TRUE) {
  sapply(name, function(tmp) {
    tmp = env[[tmp]]
    rlang::is_missing(tmp) || (or_null && is.null(tmp))
  },USE.NAMES = FALSE)
}
