#' Define an interface
#' 
#' The `iface` function allows us to define the structure of a dataframe in
#' terms of the columns and column types. An `iface` specification is used to
#' define the type of a formal parameter in a function, by being the parameters
#' default value. This definition is picked up by `ivalidate(...)` when used
#' within that function to ensure the input is correctly formatted. An interface
#' spec may also be used in `ireturn(...)` to enforce that the output of a
#' function is correct.
#' 
#' `iface` definitions can be printed and included in `roxygen` documentation
#' and help us to document input dataframe parameters and dataframe return
#' values in a standardised way.
#' 
#' The specification is in the form of a named list of formulae with the
#' structure `name = type ~ "documentation"`.
#' 
#' `type` can be one of `r .converters()` (e.g. `enum(level1,level2,...)`,
#' `in_range(min,max)`) or anything that resolves to a function e.g.
#' `as.ordered`.
#'  
#' If `type` is a function name, then the function must take a single vector
#' parameter and return a single vector of the same size. The function must also
#' return a zero length vector of an appropriate type if passed `NULL`.
#' 
#' `type` can also be a concatenation of rules separated by `+`, e.g. 
#' `integer + group_unique` for an integer that is unique within a group.
#'
#' @param ... The specification of the interface (see details), or an unnamed
#'   `iface` object to extend, or both.
#' @param .groups either `FALSE` for no grouping allowed or a formula of the form 
#'   `~ var1 + var2 + ...` which defines what columns must be grouped in the dataframe
#'   (and in which order). If `NULL` (the default) then any grouping is permitted.
#'   If the formula contains a dot e.g. `~ . + var1 + var2` then the grouping
#'   must include var1 and var2 but other groups are also allowed.
#' @param .default a default value to supply if there is nothing given in a 
#'   function parameter using the `iface` as a formal. This is either `NULL` in 
#'   which case there is no default, `TRUE` in which case the default is a zero
#'   row tibble conforming to the spec, or a provided dataframe, which is checked to 
#'   conform, and used as the default.
#'
#' @concept interface 
#'
#' @return the definition of an interface as a `iface` object
#' @export
#'
#' @examples
#' 
#' test_df = tibble::tibble(
#'   grp = c(rep("a",10),rep("b",10)), 
#'   col1 = c(1:10,1:10)
#' ) %>% dplyr::group_by(grp)
#' 
#' my_iface = iface( 
#'   col1 = integer + group_unique ~ "an integer column",
#'   .default = test_df
#' )
#' 
#' print(my_iface)
#' 
#' # the function x defines a formal `df` with default value of `my_iface`
#' x = function(df = my_iface, ...) {
#'   df = ivalidate(df,...)
#'   return(df)
#' }
#' 
#' # this works
#' x(tibble::tibble(col1 = c(1,2,3)))
#' 
#' # this fails as x is of the wrong type
#' try(x(tibble::tibble(col1 = c("a","b","c"))))
#' 
#' # this fails as x has duplicates
#' try(x(tibble::tibble(col1 = c(1,2,3,3))))
#' 
#' # this gives the default value
#' x()
#' 
#' 
#' my_iface2 = iface(my_iface, col2 = character ~ "another col", .groups = ~ col1 + col2)
#' print(my_iface2)
iface = function(..., .groups = NULL, .default = NULL) {
  dots = rlang::list2(...)
  inh = dplyr::bind_rows(dots[sapply(dots, inherits, "iface")])
  dots = dots[names(dots) != ""]
  if (length(dots) == 0) spec = tibble::tibble()
  else
    spec = tibble::tibble(
      name = names(dots),
      type = unname(lapply(dots,FUN = rlang::f_lhs) %>% as.character),
      doc = unname(sapply(dots,FUN = rlang::f_rhs))
    )
  
  # TODO: Allow ordering to be done by interleaving iface and formulae
  # using a loop.
  
  if (is.null(.groups)) {
    grps = character() 
    allw_other = TRUE
  } else if (isFALSE(.groups)) {
    grps = character() 
    allw_other = FALSE
  } else {
    grps = unique(all.vars(rlang::f_rhs(.groups)))
    allw_other = "." %in% grps
    grps=setdiff(grps,".")
  }
  
  spec2 = dplyr::bind_rows(inh,spec) %>%
    dplyr::mutate(i = dplyr::row_number()) %>%
    dplyr::group_by(name, type) %>% 
    dplyr::summarise(
      doc = .none(doc,collapse="; ",none = "<undefined>"), 
      i = min(i),
      .groups = "drop") %>%
    dplyr::arrange(i) %>%
    dplyr::select(-i)
  
  
  sc = unique(c("iface",class(spec)))
  
  tmp = structure(spec2,
                  groups = grps,
                  allow_other = allw_other,
                  default = NULL)
  
  # weird C stack usage error here if as.list.iface does not handle
  # making a list of a dataframe properly
  class(tmp)=sc
  
  if (!is.null(.default)) {
    if (isTRUE(.default)) {
      tmp2 = iproto(iface = tmp)
    } else {
      tmp2 = iconvert(.default,iface = tmp)
    }
    attr(tmp,"default") = tmp2
  }
  
  return(tmp)
}

#' Check if an object is an interface spec
#'
#' @param x the parameter to check
#' @param ... ignored
#'
#' @concept interface 
#'
#' @return a boolean.
#' @export
is.iface = function(x, ...) {
  return(inherits(x,"iface"))
}

#' Format an `iface` specification for printing
#' 
#' @inheritParams base::format
#' @exportS3Method base::format iface
#' @examples
#' my_iface = iface( 
#'   col1 = integer + group_unique ~ "an integer column"
#' )
#' 
#' print(my_iface)
#' knitr::knit_print(my_iface)
format.iface = function(x, ...) {
  grps = attr(x,"groups")
  allow_other = attr(x,"allow_other")
  default = attr(x,"default")
  if (!is.null(default)) opt = "A default value is defined."
  else  opt = "No default value."
  
  
  if (allow_other) {
    if (length(grps)==0) g = "No mandatory groupings."
    else g = sprintf("Must be grouped by: %s (and other groupings allowed).",paste0(grps,collapse = " + "))
  } else {
    if (length(grps)==0) g = "Ungrouped."
    else g = sprintf("Must be grouped by: %s (exactly).",paste0(grps,collapse = " + "))
  }
  
  paste0(c(
    "A dataframe containing the following columns: ",
    glue::glue_data(x, "* {name} ({type}) - {doc}"),
    g,
    opt
  ),collapse="\n")
}

#' @exportS3Method base::print iface
print.iface = function(x,...) {
  cat(format.iface(x))
}

#' @exportS3Method knitr::knit_print iface
knit_print.iface = function(x,...) {
  tmp = format.iface(x)
  tmp = tmp %>% 
    stringr::str_replace_all("\\n","\n\n") %>%
    stringr::str_replace_all("\\n\\n\\*","\n-")
  return(structure(tmp,class="knit_asis"))
}

#' Cast an `iface` to a plain list.
#' 
#' @exportS3Method base::as.list iface
#' 
#' @inheritParams base::as.list
#' @param flatten get a list of lists representation instead
#'   of the dataframe column by column list.
as.list.iface = function(x, ..., flatten=FALSE) {
  if (flatten) return(
    return(list(
       groups = attributes(x)$groups,
       allow_other = attributes(x)$allow_other,
       has_default = !is.null(attributes(x)$default),
       default = attributes(x)$default,
       columns = x %>% purrr::pmap(list)
    ))
  )
  NextMethod(x,...)
}

# get an iface specification from the formals
# of a function parameter definition
# this is done by getting the formal expression
# and evaluating it in the environment of the function
.get_spec = function(fn, param) {
  icall = formals(fn)[[param]]
  spec = eval(icall,envir = rlang::fn_env(fn))
  return(spec)
}

# does a iface spec have a default value?
.spec_has_default = function(spec) {
  return(!is.null(.spec_default(spec)))
}

# get the default value of an iface spec of
# use a provided value
# if a provided value check it is compliant
.spec_default = function(spec, .default=NULL) {
  if (!is.null(.default)) return(iconvert(.default,spec))
  return(attr(spec,"default"))
}

# checks a rule exists for this column
.spec_has_rule = function(spec, name) {
  return(name %in% spec$name)
}

# get the type specification for a named parameter as a string
.spec_type_of = function(spec, nm) {
  tmp = spec %>% dplyr::filter(name == nm) %>% dplyr::pull(type)
  if (length(tmp) == 0) tmp = "anything"
  return(tmp)
}

# get the documentation for a named parameter
.spec_doc_of = function(spec, name) {
  tmp = spec %>% dplyr::filter(name = name) %>% dplyr::pull(doc)
  if (length(tmp) == 0) tmp = sprintf("The type of %s is not explicitly defined.", name)
}

# what columns do we need to ensure compliance.
# anything that is mentioned in the rules but also
# any mandatory grouping colums
.spec_cols = function(spec, sym=FALSE) {
  tmp = unique(c(.spec_grps(spec),spec$name))
  if (sym) tmp = sapply(tmp, as.symbol)
  return(tmp)
}

# ?mandatory grouping columns. If none this
# will be an empty list
.spec_grps = function(spec, sym=FALSE) {
  tmp = attr(spec,"groups")
  if (sym) tmp = sapply(tmp, as.symbol)
  return(tmp)
}

# does the spec allow other columns.
# this will be a logical.
.spec_allow_other = function(spec) {
  return(attr(spec,"allow_other"))
}

# format the spec expected groups and a
# <any>+grp1+grp2
# printable string.
.spec_fmt_expected_groups = function(spec) {
  out = c(
    if(.spec_allow_other(spec)) "<any>" else NULL,
    .spec_grps(spec)
  ) %>% paste0(collapse = "+")
  if (out == "") out = "<none>"
}

# test a dataframe grouping for conformance to a spec and 
# return spec matching group columns
# throws error if not conformant
.df_spec_grps = function(df, spec, sym=FALSE) {
  dfg = dplyr::group_vars(df)
  spg = .spec_grps(spec, sym=FALSE)
  if (!identical(utils::tail(dfg,length(spg),spg))) stop("dataframe grouping does not conform.")
  return(sapply(spg,as.symbol))
}

# test a dataframe grouping  for conformance to a spec and 
# return spec matching group columns
.df_additional_grps = function(df,spec, sym=FALSE) {
  dfsg = .df_spec_grps(df,spec, sym=FALSE)
  tmp = setdiff(dplyr::group_vars(df),dfsg)
  return(sapply(tmp,as.symbol))
}

