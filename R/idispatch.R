
#' Dispatch to a named function based on the characteristics of a dataframe
#' 
#' If multiple possible dataframe formats are possible for a function, each with
#' different processing requirements the decision can be made based on a validation
#' of the input against a set of rules. The first matching rule is used to 
#' process the function. 
#'
#' @param x a dataframe
#' @param ... a set of `function name`=`interfacer::iface` pairs
#' @param .default a function to apply in the situation where none of the rules
#'   can be matched. The default results in an error being thrown.
#'
#' @return the result of dispatching the dataframe to the first function that
#'   matches the rules in `...`. Matching is permissive in that the test is
#'   passed if a dataframe can be coerced to the `iface` specified format.
#' @export
#'
#' @concept interface 
#' 
#' @examples
#' i1 = iface( col1 = integer ~ "An integer column" )
#' i2 = iface( col2 = integer ~ "A different integer column" )
#' 
#' # this is the structure for the function that woudl be exported
#' extract_mean = function(df, ...) {
#'   idispatch(df,
#'     extract_mean.i1 = i1,
#'     extract_mean.i2 = i2
#'   )
#' }
#' 
#' # this is expected to be an internal package function
#' # the naming convention here is based on S3 but it is not required
#' extract_mean.i1 = function(df = i1, ...) {
#'   message("using i1")
#'   # validation is not strictly required in this as it will already have been 
#'   # done unless this is an exported function
#'   df = ivalidate(df)
#'   mean(df$col1)
#' }
#' 
#' extract_mean.i2 = function(df = i2, uplift = 1, ...) {
#'   message("using i2")
#'   df = ivalidate(df)
#'   mean(df$col2)+uplift
#' }
#' 
#' test = tibble::tibble( col2 = 1:10 )
#' extract_mean(test, uplift = 50)
#' 
#' test2 = tibble::tibble( col1 = 1:10 )
#' extract_mean(test2, uplift = 50)
#' 
idispatch = function(x, ..., .default = NULL) {
  
  # have to dispatch using declared params from caller environment
  env = rlang::caller_env()
  # get any dots
  fn = rlang::caller_fn()
  if ("..." %in% names(formals(fn))) {
    # evaluate `...` in the caller function environment.
    tmp = do.call(rlang::list2, list(as.symbol("...")), envir = env)
    params = c(as.list(env), tmp)
  } else {
    params = as.list(env)
  }
  
  dots = rlang::list2(...)
  if (any(names(dots) == "" )) stop("all parameters must be named")
  if (!all(sapply(dots,is.iface))) stop("all `...` parameters must be `iface` specifications")
  for (i in seq_along(dots)) {
    fn_name = names(dots)[[i]]
    ifc = dots[[i]]
    if (!exists(fn_name, mode="function",envir = env)) stop("Cannot find dispatch function: ",fn_name)
    
    
    x2 = tryCatch(iconvert(x, ifc), error = function(e) NULL)
    
    if(!is.null(x2)) {
      fn = tryCatch(
        get(fn_name, mode="function",envir = env),
        error = function(e) stop("could not find function: ", fn_name)
      )
      tmp = params
      tmp[[1]] = x2
      return(do.call(fn, tmp, envir = env))
    }
    
  }
  
  if (is.null(.default)) {
    stop(
      sprintf("the `dataframe` parameter in %s(x,...) does not match any of the expected formats.",.get_fn_name(fn))
    )
  } else {
    .default = rlang::as_function(.default)
    return(.default(x))
  }
}