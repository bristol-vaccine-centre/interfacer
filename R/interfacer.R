#' Define an interface
#' 
#' This defines the structure of a dataframe.
#' The specification is in the form of a named list of formulae with 
#' the structure `name = type ~ "documentation"`. `type` can be one of
#' `r paste0("'",names(.conv),"'",collapse=",")` or `enum(level1,level2,...)` or
#' anything that resolves to a function e.g. `as.ordered`
#'
#' @param ... The specification of the interface (see details), or an unnamed
#'   `iface` object to extend, or both.
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
    type = unname(lapply(dots,FUN = rlang::f_lhs) %>% as.character),
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
    glue::glue_data(x, "* {name} ({type}) - {doc}"),""),collapse="\n")
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
#' @param .prune get rid of excess columns that are not in the spec
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
ivalidate = function(df, ..., .imap=imap(), .prune=FALSE) {
  dname = rlang::as_label(rlang::ensym(df))
  fn = rlang::caller_fn()
  .has_dots = "..." %in% names(formals(fn))
  # TODO: warn if spec names collides with formals.
  spec = .get_spec(fn, dname)
  if (inherits(df,"iface")) stop("Missing parameter, ",dname," must be supplied.\n",format(spec))
  out = iconvert(df, spec, .imap, dname, .get_fn_name(fn), .has_dots, .prune )
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

.get_spec = function(fn, param) {
  icall = formals(fn)[[param]]
  spec = eval(icall)
  return(spec)
}

.get_fn_name = function(fn) {
  if (is.null(fn)) return("<unknown>")
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
  spec = .get_spec(fn,dname)
  return(format(spec))
}


#' Convert a dataframe to a format compatible with an interface specification
#' 
#' This function is called by `ivalidate` and is not generally intended to be
#' used directly by the end user. It may be helpful in debugging during package 
#' development to know what is being used.
#'
#' @param df the dataframe to convert
#' @param iface the interface spec as an `iface`
#' @param .imap an optional `imap` mapping
#' @param .dname the name of the parameter value (optional).
#' @param .fname the name of the function (optional).
#' @param .has_dots internal library use only. Changes the nature of the error message.
#' @param .prune do you want to remove 
#'
#' @return the conformant dataframe
#' @export
#'
#' @examples
#' i_diamonds = iface( 
#'   color = enum(D,E,F,G,H,I,J,extra) ~ "the colour", 
#'   price = integer ~ "the price"
#' )
#' iconvert(ggplot2::diamonds, i_diamonds,.prune = TRUE)
iconvert = function(df, iface, .imap = interfacer::imap(), .dname="<unknown>", .fname="<unknown>", .has_dots = TRUE, .prune = FALSE) {
  dots = .imap
  spec = iface
  dots = dots[names(dots) %in% spec$name]
  out = df %>% dplyr::mutate(df, !!!dots)
  missing = dplyr::setdiff(spec$name,colnames(out))
  if (length(missing) > 0) stop(
    length(missing), " missing columns in parameter `",.dname,"` in call to ",.fname,"(...)\n",
    "consider renaming to create ",paste0("`",missing,"`",collapse = ", ")," columns\n",
    if (.has_dots) sprintf("or by adding `.imap = interfacer::imap(%s)` to your function call.\n", paste0("`",missing,"` = ???",collapse = ", ")) else ""
  )
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(spec$name))
  }
  spec %>% purrr::pwalk(.f = function(name,type,doc,...) { 
    asfn = .get_conv(type)
    out[[name]] <<- tryCatch({
      do.call(asfn, args = list(out[[name]]))
    },
    warning = function(e) stop(name," cannot be coerced to a ",type),
    error = function(e) stop(name," cannot be coerced to a ",type))
  })
  return(out)
}

#' Test dataframe conformance to an interface specification
#' 
#' `ivalidate` throws errors deliberately however sometimes dealing with invalid
#' input may be possible. 
#'
#' @param df a dataframe to test
#' @param iface an interface specification produced by `iface()`. If missing
#'   this will be inferred from the current function signature.
#' @param .imap an optional mapping specification produced by `imap()`
#'
#' @return TRUE if the dataframe is conformant, FALSE otherwise
#' @export
#'
#' @examples
#' i_diamonds = iface( 
#'   color = enum(D,E,F,G,H,I,J,extra) ~ "the colour", 
#'   price = integer ~ "the price"
#' )
#' itest(ggplot2::diamonds, i_diamonds)
itest = function(df, iface = NULL, .imap = imap()) {
  if (is.null(iface)) iface = .get_spec( rlang::caller_fn(), rlang::as_label(rlang::ensym(df)) )
  conv = tryCatch(
    iconvert(df, iface, .imap),
    error = function(e) NULL)
  return(!is.null(conv))
}

## Converters ----

# f = .get_conv("enum(x,y,z)")
# f(c("x","y"))
.get_conv = function(type) {
  if (is.function(type)) {
    f = type
  } else if (tolower(type) %in% names(.conv)) {
    f = .conv[[tolower(type)]]
  } else {
    # 
    if (stringr::str_detect(type,".+\\(.*\\)")) {
      f = try(get(paste0("as.",type)),silent = TRUE)
      if (inherits(f,"try-error")) {
        f = try(get(type),silent = TRUE)
      }
    }
    if (inherits(f,"try-error")) {
      f = eval(str2lang(type))
    }
    if (inherits(f,"try-error")) {
      stop("The type `",type,"` is not defined (there is no `as.",type,"` function.)")
    }
    if (!is.function(f)) {
      stop("The type `",type,"` should have resolved to a function.)")
    }
  }
  return(function(x) tryCatch(
    f(x),
    warning = function(e) stop(name," cannot be coerced to a ",type,": ",e$message),
    error = function(e) stop(name," cannot be coerced to a ",type,": ",e$message)
  ))
}

#' Define a factor with specific levels.
#' 
#' @param ... the levels (no quotes, backticks if required)
#' @param .drop should levels present in the data and not specified cause an error
#'  (FALSE the default) or be silently dropped to NA values (TRUE).
#'
#' @return a function that can check and convert input into the factor with specified
#'  levels. This will re-level factors with matching levels but in a different order.
#' @export
#'
#' @examples
#' f = enum(one,two,three)
#' f(c("three","two","one"))
#' f(factor(rep(1:3,5), labels = c("one","two","three")))
enum = function(..., .drop = FALSE) {
  lvls = unname(sapply(rlang::ensyms(...), rlang::as_label))
  if (length(lvls) == 0) return(.conv$factor)
  return(function(x) {
    if (is.factor(x)) {
      if (identical(levels(x), lvls)) return(x)
      x = as.character(x)
    }
    if (is.character(x)) {
      if(!all(stats::na.omit(x %in% lvls)) && !.drop) stop("values present not in levels")
      return(factor(x, lvls))
    }
    stop("enum not a factor or a character")
  })
}

.conv = list(
  integer = function(x) {
    x = as.numeric(x)
    if (any(stats::na.omit(abs(x-round(x)) > .Machine$double.eps^0.5))) stop("not a true integer input") 
    return(as.integer(x))
  },
  double = as.double,
  numeric = as.numeric,
  date = as.Date,
  logical = function(x) {
    x = as.numeric(x)
    if (any(stats::na.omit(!(x %in% c(0,1))))) stop("not a true logical input")
    return(as.logical(x))
  },
  factor = function(x) {
    if (is.factor(x)) return(x)
    factor(x)
  },
  character = as.character,
  primary_key = function(x) {
    if (any(duplicated(x))) stop("non unique primary key values")
  }
)