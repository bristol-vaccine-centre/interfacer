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
#' @param .groups either FALSE for no groups or a formula of the form `~ var1 +
#'   var2 + ...` which defines what columns must be grouped in the dataframe
#'   (and in which order). If NULL (the default) then grouping is not validated.
#'
#' @return the definition of an interface as a `iface` object
#' @export
#'
#' @examples
#' my_iface = iface( col1 = integer ~ "an integer column" )
#' print(my_iface)
#' 
#' x = function(df = my_iface, ...) {
#'   df = ivalidate(df,...)
#'   return(df)
#' }
#' 
#' x(tibble::tibble(col1 = c(1,2,3)))
#' 
#' my_iface2 = iface(my_iface, col2 = character ~ "another col", .groups = ~ col1 + col2)
#' print(my_iface2)
iface = function(..., .groups = NULL) {
  dots = rlang::list2(...)
  inh = dplyr::bind_rows(dots[sapply(dots, inherits, "iface")])
  dots = dots[names(dots) != ""]
  spec = tibble::tibble(
    name = names(dots),
    type = unname(lapply(dots,FUN = rlang::f_lhs) %>% as.character),
    doc = unname(sapply(dots,FUN = rlang::f_rhs))
  )
  
  # TODO: default values. Could be an additional term in formula?
  
  if (is.null(.groups)) 
    grps = NULL
  else if (isFALSE(.groups)) 
    grps = character() 
  else 
    grps = all.vars(rlang::f_rhs(.groups))
  
  spec = dplyr::bind_rows(inh,spec) %>%
    dplyr::group_by(name, type) %>% 
    dplyr::summarise(doc = .none(doc,collapse="; ",none = "<undefined>"), .groups = "drop")
  return(
    structure(spec,
              groups = grps,
              class=c("iface",class(spec))))
}

#' @inherit base::format
#' @export
format.iface = function(x, ...) {
  grps = attributes(x)$groups
  if (is.null(grps)) g = "Grouping undefined"
  else g = sprintf("Grouped by: %s",.none(grps,collapse = " + ", "<none>"))
  paste0(c(
    "A dataframe containing the following columns: ",
    glue::glue_data(x, "* {name} ({type}) - {doc}"),
    g
    ),collapse="\n")
}

#' @inherit base::print
#' @export
print.iface = function(x,...) {
  cat(format.iface(x))
}

#' Perform interface checks on dataframe by looking at enclosing function
#'
#' This is intended to be used within a function to check the validity of a data
#' frame parameter (usually the first parameter) against an `ispec`.
#'
#' @param df a dataframe - if missing then the first parameter of the calling
#'   function is assumed to be a dataframe.
#' @param ... not used but `ivalidate` should be included in call to inherit
#'   `.imap` from the caller function.
#' @param .imap  a set of mappings as an `imap` object.
#' @param .prune get rid of excess columns that are not in the spec.
#'
#' @return a dataframe based on df with validity checks passed and `.imap`
#'   mappings applied if present
#' @export
#'
#' @examples
#' x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
#'   df = ivalidate(...)
#'   return(df)
#' }
#' input=tibble::tibble(col1 = c(1,2,3))
#' x(input)
#'
#' # This fails because col1 is not coercable to integer
#' input2=tibble::tibble(col1 = c(1.5,2,3))
#' try(x(input2))
ivalidate = function(df = NULL, ..., .imap=imap(), .prune=FALSE) {
  dname = tryCatch(rlang::as_label(rlang::ensym(df)), error = function(e) return(NA))
  if (is.na(dname)) {
    df = .get_first_param_value()
    dname = .get_first_param_name()
  }
  
  fn = rlang::caller_fn()
  .has_dots = "..." %in% names(formals(fn))
  # TODO: warn if spec names collides with formals.
  spec = .get_spec(fn, dname)
  if (inherits(df,"iface")) stop("Missing parameter, ",dname," must be supplied.\n",format(spec))
  out = iconvert(df, spec, .imap, dname, .get_fn_name(fn), .has_dots, .prune )
  return(out)
}

#' Perform interface checks on dataframe by looking at enclosing function
#'
#' This is intended to be used within a function to check the validity of a data
#' frame parameter (usually the first parameter) against an `ispec`.
#'
#' @param df a dataframe - if missing then the first parameter of the calling
#'   function is assumed to be a dataframe.
#' @param iface the interface specification that `df` should conform to.
#' @param .prune get rid of excess columns that are not in the spec.
#'
#' @return a dataframe based on df with validity checks passed and `.imap`
#'   mappings applied if present
#' @export
#'
#' @examples
#' input = iface(col_in = integer ~ "an integer column" )
#' output = iface(col_out = integer ~ "an integer column" )
#' x = function(df = input, ...) {
#'   df = ivalidate(...)
#'   tmp = df %>% dplyr::rename(col_out = col_in)
#'   ireturn(tmp, output)
#' }
#' x(tibble::tibble(col_in = c(1,2,3)))
#' output
ireturn = function(df, iface, .prune=FALSE) {
  spec = iface
  fn = rlang::caller_fn()
  .fname = .get_fn_name(fn)
  out = df
  grps = attributes(spec)$groups
  if (!is.null(grps)) {
    if(!identical(dplyr::group_vars(out),grps)) {
      expect = .none(grps, collapse = " + ")
      obs = .none(dplyr::group_vars(out), collapse = " + ")
      warning(
        sprintf("%s(...): output dataframe grouping (%s) does not match expectations (%s)\n", .fname, obs, expect)
      )
      out = out  %>% dplyr::group_by(dplyr::across(tidyselect::all_of(grps)))
    }
  }
  
  missing = dplyr::setdiff(spec$name,colnames(out))
  if (length(missing) > 0) stop(
    sprintf("missing columns (%s) in return value of %s(..)", .none(missing,","),.fname)
  )
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(unique(c(spec$name,grps))))
  }
  spec %>% purrr::pwalk(.f = function(name,type,doc,...) { 
    asfn = .get_conv(type)
    out[[name]] <<- tryCatch({
      do.call(asfn, args = list(out[[name]]))
    },
    warning = function(e) stop(
      sprintf("output column `%s` in function `%s(...)` cannot be coerced ",name,.fname),
      e$message), 
    error = function(e) stop(
      sprintf("output column `%s` in function `%s(...)` cannot be coerced ",name,.fname),
      e$message) 
    )
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
#' This function is expected to be called within the documentation of a function
#' as inline code in the parameter documentation of the function. It details the
#' expected columns that the input dataframe should possess.
#'
#' @param fn the function that you are documenting
#' @param param the parameter you are documenting (optional. if missing defaults
#'   to the first argument of the function)
#'
#' @return a markdown snippet
#' @export
#'
#' @examples
#' #' @param df `r idocument(x, df)`
#' x = function(df = iface( col1 = integer ~ "an integer column" )) {}
#'
#' cat(idocument(x, df))
idocument = function(fn, param = NULL) {
  dname = tryCatch(rlang::as_label(rlang::ensym(param)), error = function(e) NA)
  if (is.na(dname)) {
    dname = names(formals(fn))[[1]]
  }
  spec = .get_spec(fn,dname)
  return(format(spec))
}

.none = function(x, collapse = ", ", none = "<none>", fmt = "%s") {
  if (is.null(x)) return(none)
  if (length(x) == 0) return(none)
  return(sprintf(fmt,paste0(x, collapse = collapse)))
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
  out = df %>% dplyr::mutate(!!!dots)
  
  grps = attributes(spec)$groups
  if (!is.null(grps)) {
    if(!identical(dplyr::group_vars(out),grps)) {
      expect = .none(grps, collapse = " + ")
      obs = .none(dplyr::group_vars(out), collapse = " + ")
      exp_grp = .none(grps, collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
      diff = dplyr::setdiff(dplyr::group_vars(out), grps)
      diff_grp = .none(diff, collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
      exp_grp_2 = .none(grps, collapse = ",", none = "", fmt = " %%>%% group_by(%s)")
      if(identical(dplyr::group_vars(out),diff)) diff_grp_2 = ""
      else diff_grp_2 = .none(diff, collapse = ",", none = "", fmt = " %%>%% group_by(%s)")
      messages = c(
        sprintf("the specified dataframe grouping (%s) does not match actual grouping (%s)\n", expect, obs),
        sprintf("consider regrouping your data before calling function `%s`, e.g.:\n",.fname),
        sprintf("`df %s %%>%% %s(...)`\n", exp_grp, .fname)
      )
      if (length(diff) > 0) {
        messages = c(
          messages,
          sprintf("or calling function `%s` using a group_modify, e.g.:\n",.fname),
          sprintf("`df%s %%>%% group_modify(function(d,g,...) {%s(%s=d%s, ...)})`", diff_grp_2, .fname, .dname, exp_grp_2)
        )
      }
      stop(messages)
    }
  }
  
  missing = dplyr::setdiff(spec$name,colnames(out))
  if (length(missing) > 0) stop(
    length(missing), " missing columns in parameter `",.dname,"` in call to ",.fname,"(...)\n",
    "consider renaming to create ",paste0("`",missing,"`",collapse = ", ")," columns\n",
    if (.has_dots) sprintf("or by adding `.imap = interfacer::imap(%s)` to your function call.\n", paste0("`",missing,"` = ???",collapse = ", ")) else ""
  )
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(unique(c(spec$name),grps)))
  }
  spec %>% purrr::pwalk(.f = function(name,type,doc,...) { 
    asfn = .get_conv(type)
    out[[name]] <<- tryCatch({
      do.call(asfn, args = list(out[[name]]))
    },
    warning = function(e) stop(
      sprintf("input column `%s` in function parameter `%s(%s = ?)` cannot be coerced ",name,.fname,.dname),
      e$message), 
    error = function(e) stop(
      sprintf("input column `%s` in function parameter `%s(%s = ?)` cannot be coerced ",name,.fname,.dname),
      e$message) 
    )
  })
  return(out)
}

#' Test dataframe conformance to an interface specification.
#' 
#' `ivalidate` throws errors deliberately however sometimes dealing with invalid
#' input may be desirable. `itest` is generally designed to be used within a function which
#' specifies the expected input using `iface`, and allows the function to test if
#' its given input is conformant to the interface.
#'
#' @param df a dataframe to test. If missing the first parameter of the calling
#'   function is assumed to be the dataframe to test.
#' @param iface an interface specification produced by `iface()`. If missing
#'   this will be inferred from the current function signature.
#' @param .imap an optional mapping specification produced by `imap()`
#'
#' @return TRUE if the dataframe is conformant, FALSE otherwise
#' @export
#'
#' @examples
#' if (rlang::is_installed("ggplot2")) {
#'   i_diamonds = iface( 
#'     color = enum(D,E,F,G,H,I,J,extra) ~ "the colour", 
#'     price = integer ~ "the price"
#'   )
#'   
#'   # Ad hoc testing
#'   itest(ggplot2::diamonds, i_diamonds)
#'   
#'   # Use within function:
#'   x = function(df = i_diamonds) {
#'     if(itest()) message("PASS!")
#'   }
#'   
#'   x(ggplot2::diamonds)
#' }
itest = function(df = NULL, iface = NULL, .imap = imap()) {
  
  if (is.null(iface)) {
    dname = tryCatch(rlang::as_label(rlang::ensym(df)), error = function(e) return(NA))
    if (is.na(dname)) {
      df = .get_first_param_value()
      dname = .get_first_param_name()
    }
    iface = .get_spec( rlang::caller_fn(), dname )
  }
  
  df = force(df)
  conv = tryCatch(
    iconvert(df, iface, .imap),
    error = function(e) NULL)
  return(!is.null(conv))
}

# only works for itest and ivalidate, anything else will be wrong depth
.get_first_param_name = function() {
  names(formals(sys.function(-2)))[[1]]
}

# only works for itest and ivalidate, anything else will be wrong depth
.get_first_param_value = function() {
  first_name = names(formals(sys.function(-2)))[[1]]
  value = sys.frame(-2)[[first_name]]
  return(value)
}


## Converters ----

# f = .get_conv("enum(x,y,z)")
# f(c("x","y"))
# parses the type and returns a function that can validate the input.
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
    warning = function(e) stop("to a ",type,": ",e$message),
    error = function(e) stop("to a ",type,": ",e$message)
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