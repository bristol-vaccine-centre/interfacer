#' Checks a set of variables can be coerced to numeric and coerces them
#' 
#' N.B. This only works for the specific environment (to prevent weird side effects)
#'
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#'
#' @concept parameter_checks
#'
#' @examples
#' a = c(1:4L)
#' b = c("1",NA,"3.3")
#' f = NULL
#' g = NA
#' check_numeric(a,b,f,g)
#' 
#' c = c("dfsfs")
#' try(check_numeric(c,d, mean))
check_numeric = function(..., .message="`{param}` is non-numeric ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.numeric(.x)
  convert = type.numeric
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}


#' Checks a set of variables can be coerced to integer and coerces them
#' 
#' N.B. This only works for the specific environment (to prevent weird side effects)
#'
#' @param ... a list of symbols
#' @param .message a glue specification containing `{param}` as the name of the
#'   parameter and `{err}` the cause of the error
#' @param .env the environment to check (defaults to calling environment)
#'
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept parameter_checks
#'
#' @examples
#' a = c(1:4)
#' b = c("1",NA,"3")
#' f = NULL
#' g = NA
#' check_integer(a,b,f,g)
#' 
#' c = c("dfsfs")
#' e = c(1.0,2.3)
#' try(check_integer(c,d,e, mean))
check_integer = function(..., .message="`{param}` is not an integer ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.integer(.x)
  convert = type.integer
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}



#' Checks a set of variables can be coerced to a date and coerces them
#' 
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept parameter_checks
#'
#' @examples
#' a = c(Sys.Date()+1:10)
#' b = format(a)
#' f = "1970-01-01"
#' g = NA
#' check_date(a,b,f,g)
#' 
#' c = c("dfsfs")
#' try(check_date(c,d, mean))
check_date = function(..., .message="`{param}` is not a date: ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is(.x, "Date")
  convert = type.date
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}


#' Checks a set of variables can be coerced to a logical and coerces them
#' 
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept parameter_checks
#'
#' @examples
#' a = c("T","F")
#' b = c(1,0,1,0)
#' f = TRUE
#' g = NA
#' check_logical(a,b,f,g)
#' 
#' c = c("dfsfs")
#' try(check_logical(c,d, mean))
check_logical = function(..., .message="`{param}` is not a logical: ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.logical(.x)
  convert = type.logical
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}


#' Checks a set of variables can be coerced to a character and coerces them
#' 
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept parameter_checks
#'
#' @examples
#' a = c(Sys.Date()+1:10)
#' b = format(a)
#' f = iris$Species
#' g = NA
#' check_character(a,b,f,g)
check_character = function(..., .message="`{param}` is not a character: ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.character(.x)
  convert = type.character
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}


#' Checks a set of variables are all of length one
#' 
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept parameter_checks
#'
#' @examples
#' a = 1
#' b = "Hello"
#' g = NA
#' check_single(a,b,g)
#' 
#' c= c(1,2,3)
#' d=list(a,b)
#' try(check_single(c,d,missing))
check_single = function(..., .message="`{param}` is not length one: ({err}).", .env = rlang::caller_env()) {
  predicate = ~ length(.x) == 1
  convert = function(x,...) {
    if (is.null(x)) stop("NULL value is not allowed",call. = FALSE)
    if (length(x)>1) stop("list/vector input not allowed",call. = FALSE)
    return(x)
  }
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}
