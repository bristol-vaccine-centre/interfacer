# TODO: named arguments as parameters for convert
.check_framework = function(..., predicate, convert, .message, .env) {
  
  exprs = rlang::enexprs(...)
  
  vars = exprs[names(exprs) == ""]
  params = exprs[names(exprs) != ""]
  if (length(params) == 0) params=NULL
  
  env = .env
  errors = character()
  warnings = character()
  predicate = rlang::as_function(predicate)
  convert = rlang::as_function(convert)
  for (i in seq_along(vars)) {
    # i = 1
    focus = rlang::as_name(vars[[i]])
    if (!.exists(focus, env)) {
      warnings = c(warnings,sprintf("'%s' is not defined in this context", focus))
    } else if (rlang::is_missing(env[[focus]])) {
      # We may resolve missing variables later, and this may require the other
      # variables being coerced. We do nothing and say nothing.
    } else {
      tmp = .get(focus, env)
      if (!is.null(tmp)) {
        if (is.function(tmp)) {
          errors = c(errors,sprintf("'%s' is a function", focus))
        } else {
          if (!predicate(tmp)) {
            params = purrr::map(params,eval)
            tmp2 = try(rlang::exec(convert, tmp, !!!params),silent = TRUE)
            
            if (inherits(tmp2, "try-error")) {
              param = focus
              err = attr(tmp2,"condition")$message
              errors = c(errors,glue::glue(.message))
            } else {
              env[[focus]] = tmp2
            }
            
          }
        }
      }
    }
  }
  if (length(warnings) > 0) warning(paste0(1:length(warnings), ") ", warnings,collapse = "\n"))
  if (length(errors) > 0) stop(paste0(1:length(errors), ") ", errors,collapse = "\n"))
  invisible(env)
}


.exists = function(name, env, ...) {
  if (is.data.frame(env)) {
    return(as.character(name) %in% colnames(env))
  } else {
    return(exists(name,envir = env, inherits = FALSE))
  }
} 

.get = function(name, env) {
  if (is.data.frame(env)) {
    return(env[[as.character(name)]])
  } else {
    return(get(name,envir = env, inherits = FALSE))
  }
}