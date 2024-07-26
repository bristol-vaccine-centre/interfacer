
# collapse a character() into a single value with a default for empty value
.none = function(x, collapse = ", ", none = "<none>", fmt_item = "%s", fmt = "%s") {
  if (is.null(x)) return(none)
  if (length(x) == 0) return(none)
  return(sprintf(fmt, paste0(sprintf(fmt_item,x), collapse = collapse)))
}


# like rlang::as_function but only interprets functions or formulae
# and ignores primitives and characters.
.lax_as_function = function(fn) {
  try(if (is.function(fn)) return(fn), silent = TRUE)
  try(if (rlang::is_formula(fn)) return(rlang::as_function(fn)), silent = TRUE)
  return(fn)
}


#' Check for existence of a set of columns in a dataframe
#'
#' @param df a dataframe to test
#' @param ... the column names (unquoted)
#'
#' @return TRUE if the columns are all there, false otherwise
#' @export
#'
#' @examples
#' is_col_present(iris, Species, Petal.Width)
is_col_present = function(df, ...) {
  cols=rlang::ensyms(...)
  if (length(cols) == 0) return(FALSE)
  return(all(sapply(cols, rlang::as_label) %in% colnames(df)))
}


#' Execute a function or return a value if a column in present in a dataframe
#' 
#' The simple use case. For more complex behaviour see `switch_pipeline()`.
#'
#' @param df a dataframe
#' @param col a column name
#' @param if_present a `purrr` style function to execute on the dataframe if the
#'   column is present (or a plain value)
#' @param if_missing a `purrr` style function to execute on the dataframe if the
#'   column is missing (or a plain value)
#'
#' @return either the value of `if_present`/`if_absent` or the result of calling
#'   `if_present`/`if_absent` as functions on `df`.
#' @export
#'
#' @examples
#' iris %>% if_col_present(Species, ~ .x %>% dplyr::rename(new = Species)) %>%
#'   colnames()
#' 
#' # in contrast to `purrr` absolute values are not interpreted as function names  
#' iris %>% if_col_present(Species2, "Yes", "No")
#' 
if_col_present = function(df, col, if_present, if_missing = ~ .x) {
  if (is_col_present(df, {{col}})) {
    fn = .lax_as_function(if_present)
  } else {
    fn = .lax_as_function(if_missing) 
  }
  
  if (rlang::is_function(fn)) {
    return(fn(df))
  } else {
    return(fn)
  }
}




#' Branch a `dplyr` pipeline based on a set of conditions
#'
#' @param .x a dataframe
#' @param ... a list of formulae of the type `predicate ~ purrr function` using 
#'   `.x` as the single parameter
#'
#' @return the result of applying `purrr function` to `.x` in the case where
#'   `predicate` evaluates to true. Both predicate and function can refer to 
#'   the pipeline dataframe using `.x`
#' @export
#'
#' @examples
#' 
#' iris %>% switch_pipeline(
#'   is_col_present(.x, Species) ~ .x %>% dplyr::rename(new = Species)
#' ) %>% dplyr::glimpse()
switch_pipeline = function(.x, ...) {
  forms = rlang::list2(...)
  if (!all(sapply(forms,rlang::is_formula))) stop("all parameters must be formulae", call. = FALSE)
  if (any(names(forms) != "")) stop("all parameters must be unnamed", call. = FALSE)
  for (form in forms) {
    predicate = rlang::f_lhs(form)
    # browser()
    fn = rlang::as_function(rlang::f_rhs(form) %>% stats::asOneSidedFormula())
    if(isTRUE(tryCatch(eval(predicate), error=function(e) FALSE))) return(fn(.x))
  }
  return(.x)
}