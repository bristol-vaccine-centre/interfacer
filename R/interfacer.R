#' Define an interface
#' 
#' This is the default value for a dataframe.
#' The specificiation is in the form of a named list of formulae with 
#' the structure `name = type ~ "documentation"`
#'
#' @param ... The specification of the interface (see details)
#'
#' @return the definition of an interface as a `iface` object
#' @export
#'
#' @examples
#' my_iface = iface(col1 = integer ~ "an integer column" )
#' print(my_iface)
#' 
#' x = function(df = my_iface, ...) {
#'   df = ivalidate(df,...)
#'   return(df)
#' }
#' 
#' x(tibble::tibble(col1 = c(1,2,3)))
#' 
#' my_iface2 = iface(my_iface, col2 = character ~ "another col")
#' print(my_iface2)
iface = function(...) {
  dots = rlang::list2(...)
  inh = dplyr::bind_rows(dots[sapply(dots, inherits, "iface")])
  dots = dots[names(dots) != ""]
  spec = tibble::tibble(
    name = names(dots),
    type = unname(lapply(dots,FUN = rlang::f_lhs) %>% sapply(FUN=as.character)),
    doc = unname(sapply(dots,FUN = rlang::f_rhs))
  )
  spec = dplyr::bind_rows(inh,spec) %>%
    dplyr::group_by(name, type) %>% 
    dplyr::summarise(doc = paste0(doc,collapse="; "),.groups = "drop")
  return(structure(spec,class=c("iface",class(spec))))
}

#' @inherit base::format
#' @export
format.iface = function(x, ...) {
  paste0(c(
    "A dataframe containing the following columns: ",
    glue::glue_data(x, "* {name} ({type}} - {doc}"),""),collapse="\n")
}

#' @inherit base::print
#' @export
print.iface = function(x,...) {
  cat(format.iface(x))
}

#' Perform interface checks on dataframe by looking at enclosing function
#'
#' @param df a dataframe
#' @param ... not used
#' @param .imap  a set of mappings as an `imap` object
#' @param prune get rid of excess columns that are not in the spec
#'
#' @return a dataframe based on df with 
#' @export
#'
#' @examples
#' x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
#'   df = ivalidate(df,...)
#'   return(df)
#' }
#' input=tibble::tibble(col1 = c(1,2,3)) 
#' x(input)
#' 
#' # This fails because col1 is not coercable to integer
#' input2=tibble::tibble(col1 = c(1.5,2,3)) 
#' try(x(input2))
ivalidate = function(df, ..., .imap=imap(), prune=FALSE) {
  dname = rlang::as_label(rlang::ensym(df))
  dots = .imap
  fn = rlang::caller_fn()
  icall = formals(fn)[[dname]]
  spec = eval(icall)
  if (inherits(df,"iface")) stop("Missing parameter, ",dname," must be supplied.\n",format(spec))
  # TODO: warn if spec names collides with formals.
  dots = dots[names(dots) %in% spec$name]
  out = df %>% dplyr::mutate(df, !!!dots)
  missing = dplyr::setdiff(spec$name,colnames(out))
  has_dots = "..." %in% names(formals(fn))
  # missing=c("missing","missing2")
  if (length(missing) > 0) stop(
    length(missing), " missing columns in parameter `",dname,"` in call to ",.get_fn_name(fn),"(...)\n",
    "consider renaming to create ",paste0("`",missing,"`",collapse = ", ")," columns\n",
    if (has_dots) sprintf("or by adding `.imap = interfacer::imap(%s)` to your function call.\n", paste0("`",missing,"` = ???",collapse = ", ")) else ""
  )
  if (prune) {
    out = out %>% dplyr::select(tidyselect::all_of(spec$name))
  }
  spec %>% purrr::pwalk(.f = function(name,type,doc,...) { 
    asfn = paste0("as.",type)
    out[[name]] <<- tryCatch({
        do.call(asfn, args = list(out[[name]]))
      },
      warning = function(e) stop(name," cannot be coerced to a ",type),
      error = function(e) stop(name," cannot be coerced to a ",type))
  })
  return(out)
}

#' Specify mappings that can make dataframes compatible with an interface
#' 
#' This function is expected to be used only in a `.imap = imap(...)` context
#' to overcome some mapping issues 
#'
#' @param ... a set of `dplyr::mutate()` specifications that when applied to
#' a dataframe will rename or otherwise fix missing columns
#'
#' @return a set of mappings
#' @export
#'
#' @examples
#' x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
#'   df = ivalidate(df,...)
#' }
#' input=tibble::tibble(col2 = c(1,2,3)) 
#' # This fails because col1 is missing
#' try(x(input))
#' # This fixes it for this input
#' x(input, .imap=imap(col1 = col2))
imap = function(...) {
  tmp = rlang::enexprs(...)
  return(structure(
    tmp,
    class = c("imap",class(tmp))
  ))
}

.get_fn_name = function(fn) {
  fnenv= as.list(rlang::fn_env(fn))
  matches = sapply(fnenv, function(x) isTRUE(all.equal(x,fn)))
  if (any(matches)) return(paste0(names(fnenv)[matches],collapse = "/"))
  return("<unknown>")
}

#' Document an interface contract for inserting in to Roxygen 
#'
#' This function is expected to be called within the documentation of a 
#' function as inline code in the parameter documentation of the function. It
#' details the expected columns that the input dataframe should possess.
#'
#' @param fn the function that you are documenting
#' @param param the parameter you are documenting
#'
#' @return a markdown snippet
#' @export
#'
#' @examples
#' #' @param df `r idocument(x, df)`
#' x = function(df = iface( col1 = integer ~ "an integer column" )) {}
#' 
#' cat(idocument(x, df))
idocument = function(fn, param) {
  dname = rlang::as_label(rlang::ensym(param))
  icall = formals(fn)[[dname]]
  spec = eval(icall)
  return(format(spec))
}


