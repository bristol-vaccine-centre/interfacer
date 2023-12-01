#' Define an interface
#' 
#' The `iface` function allows us to define the structure of a dataframe in
#' terms of the columns and column types. An `iface` specification is used to
#' define the type of a formal parameter in a function, by being assigned as its
#' default value. This definition is picked up by `ivalidate(...)` within that
#' function to ensure the input is correctly formatted. An interface spec may
#' also be used in `ireturn(...)` to enforce that the output of a function is 
#' correct
#' 
#' `iface` definitions can be printed and included in `roxygen` documentation
#' and help us to document input dataframe parameters and dataframe return values
#' in a standardised way.
#' 
#' The specification is in the form of a named list of formulae with 
#' the structure `name = type ~ "documentation"`. `type` can be one of
#' `r paste0("'",names(.conv),"'",collapse=",")` or 'enum(level1,level2,...)',
#' 'in_range(min,max)' or anything that resolves to a function e.g. 'as.ordered'.
#' If a function it must take a single vector parameter and return a single
#' vector of the same size. The function must return a zero length vector of an
#' appropriate type if passed `NULL`. `type` can also be a concatenation of
#' rules
#'
#' @param ... The specification of the interface (see details), or an unnamed
#'   `iface` object to extend, or both.
#' @param .groups either FALSE for no groups or a formula of the form `~ var1 +
#'   var2 + ...` which defines what columns must be grouped in the dataframe
#'   (and in which order). If NULL (the default) then grouping is not validated.
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
#' my_iface = iface( 
#'   col1 = integer + group_unique ~ "an integer column",
#'   .default = tibble::tibble(col1 = 1:10)
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
  
  tmp = structure(spec,
              groups = grps,
              default = NULL,
              class=c("iface",class(spec)))
  
  if (!is.null(.default)) {
    if (isTRUE(.default)) {
      .default = iproto(iface = tmp)
    } else {
      .default = iconvert(.default,iface = tmp)
    }
    attr(tmp,"default") = .default
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

#' @inherit base::format
#' @export
format.iface = function(x, ...) {
  grps = attr(x,"groups")
  default = attr(x,"default")
  if (!is.null(default)) opt = "An optional"
  else  opt = "Required"
  
  if (is.null(grps)) g = "Optionally grouped"
  else if (length(grps)==0) g = "Ungrouped"
  else g = sprintf("Grouped by: %s",.none(grps,collapse = " + ", "<none>"))
  paste0(c(
    sprintf("%s dataframe containing the following columns: ",opt),
    glue::glue_data(x, "* {name} ({type}) - {doc}"),
    g
    ),collapse="\n")
}

#' @inherit base::print
#' @export
print.iface = function(x,...) {
  cat(format.iface(x))
}

#' @importFrom knitr knit_print
#' @inherit knitr::knit_print
#' @export
knit_print.iface = function(x,...) {
  default = attributes(x)$default
  grps = attributes(x)$groups
  if (is.null(grps)) g = "\nGrouping undefined"
  else if (length(grps)==0) g = "\nUngrouped"
  else g = sprintf("\nGrouped by: %s",.none(grps,collapse = " + ", "none"))
  
  if (!is.null(default)) opt = "An optional"
  else  opt = "Required"
  
  tmp = paste0(c(
    sprintf("%s dataframe containing the following columns: \n",opt),
    glue::glue_data(x, "- {name} ({type}) - {doc}"),
    g
  ),collapse="\n")
  return(structure(tmp,class="knit_asis"))
}

#' Perform interface checks on dataframe by looking at enclosing function formals
#'
#' `ivalidate(...)` is intended to be used within a function to check the validity of a data
#' frame parameter (usually the first parameter) against an `ispec` which is 
#' given as a default value of a formal parameter.
#'
#' @param df a dataframe - if missing then the first parameter of the calling
#'   function is assumed to be a dataframe.
#' @param ... not used but `ivalidate` should be included in call to inherit
#'   `.imap` from the caller function.
#' @param .imap  a set of mappings as an `imapper` object.
#' @param .prune get rid of excess columns that are not in the spec.
#' @param .default a default dataframe conforming to the specification. This
#'   overrides any defaults defined in the interface specification
#'
#' @return a dataframe based on df with validity checks passed and `.imap`
#'   mappings applied if present
#' @export
#' 
#' @concept interface 
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
ivalidate = function(df = NULL, ..., .imap=imapper(), .prune=FALSE, .default = NULL) {
  dname = tryCatch(rlang::as_label(rlang::ensym(df)), error = function(e) return(NA))
  fn = rlang::caller_fn()
  if (is.null(fn)) stop("`ivalidate` must be called from within an enclosing function. did you mean to use `iconvert`?",call. = FALSE)
  if (is.na(dname)) {
    df = .get_first_param_value()
    dname = .get_first_param_name()
  }
  
  
  .has_dots = "..." %in% names(formals(fn))
  # TODO: warn if spec names collides with formals.
  spec = .get_spec(fn, dname)
  if (is.iface(df)) {
    # any provided default
    if (!is.null(.default)) {
      return(iconvert(.default, spec))
    }
    # any iface level default.
    if (!is.null(attr(spec,"default"))) {
      return(attr(spec,"default"))
    }
    stop("Missing parameter, ",dname," must be supplied.\n",format(spec))
  }
  out = iconvert(df, spec, .imap, dname, .get_fn_name(fn), .has_dots, .prune )
  return(out)
}

#' Check a return parameter from a function
#'
#' This is intended to be used within a function to check the validity of a data
#' frame being returned from a function against an `ispec` which is provided.
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
#' @concept interface 
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
  out = out %>% .coerce(spec, .fname)
  return(out)
}
  


#' Dispatch to a named function based on the characteristics of a dataframe
#' 
#' If multiple possible dataframe formats are possible for a function, each with
#' different processing requirements the decision can be made based on a validation
#' of the input against a set of rules. The first matching rule is used to 
#' process the function. 
#'
#' @param x a dataframe
#' @param ... a set of `function name`=`interfacer::iface` pairs
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
idispatch = function(x, ...) {
  
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
    out = tryCatch({
      x2 = iconvert(x, ifc)
      fn = get(fn_name, mode="function",envir = env)
      tmp = params
      tmp$x = x2
      return(do.call(fn, tmp, envir = env))
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  stop("the dataframe does not match any of the expected formats.")
}

#' Specify mappings that can make dataframes compatible with an interface
#' 
#' This function is expected to be used only in a `.imap = imappper(...)` context
#' to overcome some mapping issues 
#'
#' @param ... a set of `dplyr::mutate()` specifications that when applied to
#' a dataframe will rename or otherwise fix missing columns
#'
#' @return a set of mappings
#' @export
#'
#' @concept interface 
#'
#' @examples
#' x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
#'   df = ivalidate(df,...)
#' }
#' input=tibble::tibble(col2 = c(1,2,3)) 
#' # This fails because col1 is missing
#' try(x(input))
#' # This fixes it for this input
#' x(input, .imap=imapper(col1 = col2))
imapper = function(...) {
  tmp = rlang::enexprs(...)
  return(structure(
    tmp,
    class = c("imapper",class(tmp))
  ))
}

#' Generate a zero length tibble conforming to a spec
#'
#' @param iface the specification
#'
#' @return a tibble conforming to `iface`
#' @export
#'
#' @concept interface 
#'
#' @examples
#' i = interfacer::iface(
#'   col1 = integer ~ "A number",
#'   col2 = character ~ "A string"
#' )
#' 
#' iproto(i)
iproto = function(iface) {
  out = tibble::tibble()
  iface %>% purrr::pwalk(.f = function(name,type,doc,...) { 
    # add empty column
    out <<- out %>% dplyr::mutate(!!name := .get_conv(type)(NULL))
  })
  return(out)
}

.get_spec = function(fn, param) {
  icall = formals(fn)[[param]]
  spec = eval(icall,envir = rlang::fn_env(fn))
  return(spec)
}

.get_fn_name = function(fn) {
  if (is.null(fn)) return("<unknown>")
  fnenv= as.list(rlang::fn_env(fn))
  matches = sapply(fnenv, function(x) isTRUE(all.equal(x,fn)))
  if (any(matches)) return(paste0(names(fnenv)[matches],collapse = "/"))
  return("<unknown>")
}

#' Document an interface contract for inserting into `roxygen`
#'
#' This function is expected to be called within the documentation of a function
#' as inline code in the parameter documentation of the function. It details the
#' expected columns that the input dataframe should possess.
#'
#' @param fn the function that you are documenting
#' @param param the parameter you are documenting (optional. if missing defaults
#'   to the first argument of the function)
#'
#' @concept document
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
  if (is.iface(fn)) {
    spec = fn
  } else {
    dname = tryCatch(rlang::as_label(rlang::ensym(param)), error = function(e) NA)
    if (is.na(dname)) {
      dname = names(formals(fn))[[1]]
    }
    spec = .get_spec(fn,dname)
  }
  return(knit_print.iface(spec))
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
#' @concept interface
#'
#' @param df the dataframe to convert
#' @param iface the interface spec as an `iface`
#' @param .imap an optional `imapper` mapping
#' @param .dname the name of the parameter value (optional).
#' @param .fname the name of the function (optional).
#' @param .has_dots internal library use only. Changes the nature of the error message.
#' @param .prune do you want to remove non matching columns?
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
iconvert = function(df, iface, .imap = interfacer::imapper(), .dname="<unknown>", .fname="<unknown>", .has_dots = TRUE, .prune = FALSE) {
  dots = .imap
  spec = iface
  if (!is.iface(spec)) stop("iface must be a `interfacer::iface(...) specification")
  
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
    if (.has_dots) sprintf("or by adding `.imap = interfacer::imapper(%s)` to your function call.\n", paste0("`",missing,"` = ???",collapse = ", ")) else ""
  )
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(unique(c(spec$name),grps)))
  }
  out = out %>% .coerce(spec,.fname,.dname)
  return(out)
}

# use a spec to coerce a dataframe to the correct value
# this does not apply conversion groupwise. 
.coerce = function(grp_df, spec, .fname, .dname="<unknown>") {
  
  grp_df %>% dplyr::group_modify(function(df,g,...) {
    
    
    
    spec %>% purrr::pwalk(.f = function(name,type,doc,...) { 
      asfn = .get_conv(type, .fname, .dname)
      
      df[[name]] <<- tryCatch({
        do.call(asfn, args = list(df[[name]]))
      },
      warning = function(e) stop(
        if (!is.null(.dname)) {
          sprintf("input column `%s` in function parameter `%s(%s = ?)` cannot be coerced ",name,.fname,.dname)
        } else {
          sprintf("output column `%s` in return value from `%s(...)` cannot be coerced ",name,.fname)
        },
        e$message,call. = FALSE), 
      error = function(e) stop(
        if (!is.null(.dname)) {
          sprintf("input column `%s` in function parameter `%s(%s = ?)` cannot be coerced ",name,.fname,.dname)
        } else {
          sprintf("output column `%s` in return value from `%s(...)` cannot be coerced ",name,.fname,.dname)
        },
        e$message,call. = FALSE) 
      )
    })
    return(df)
    
  })
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
#' @param .imap an optional mapping specification produced by `imapper()`
#'
#' @return TRUE if the dataframe is conformant, FALSE otherwise
#' @export
#'
#' @concept interface
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
itest = function(df = NULL, iface = NULL, .imap = imapper()) {
  
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

.list_test = function(type) {
  f = .get_conv(type)
  return(function(x) {
    if (is.null(x)) return(list())
    if (!is.list(x)) stop("not a list column")
    lapply(x, f)
  })
}

.nested_iface = function(iface, .fname, .dname) {
  return(
    function(x) {
      if (is.null(x)) return(iproto(iface))
      return(iconvert(x, 
        iface, 
        .fname=sprintf("nested %s",.fname), 
        .dname=sprintf("nested %s",.dname),
        .prune = FALSE)
      )
    }
  )
}

# f = .get_conv("enum(x,y,z)")
# f(c("x","y"))
# parses the type and returns a function that can validate the input.
# g = .get_conv("as.list")
# g(c(1,2,3))
# f2 = .get_conv("primary_key + integer")
# f2(c(1:10,1:10))
# f3 = .get_conv("list(integer)")
# f3(list(c(1,2,3), c(4,5)))
.get_conv = function(type, .fname, .dname) {
  orig = type
  types = stringr::str_split(type, "\\s?\\+\\s?")[[1]]
  
  fs = list()
  for (type in types) {
    if (is.function(type)) {
      f = type
    } else if (tolower(type) %in% names(.conv)) {
      f = .conv[[tolower(type)]]
    } else {
      # list(integer)
      # list(itest)
      if (exists(type) && is.iface(get(type))) {
        
        # type is an iface
        f = .nested_iface(get(type), .fname, .dname)
        
      } else if (stringr::str_detect(type,"list\\(.+\\)")) {
        type2 = stringr::str_extract(type,"list\\((.+)\\)",1)
        f = .list_test(type2)
      } else if (stringr::str_detect(type,".+\\(.*\\)")) {
        
        # the type is specified as something like `xyx(type)`
        # this enables us to define functions like in_range(1,2)
        # which are function generators such that in_range(1,2)(0:3) does 
        # something or throws an error.
        f = try(eval(str2lang(type)),silent = TRUE)
        # in theory this also allows us to define an iface inline
        if (is.iface(f)) f=.nested_iface(f, .fname, .dname)
        if (inherits(f,"try-error")) stop("Couldn't evaluate the type: ",type,"\nBecause of ",attr(f,"condition")$message)
      } else {
        # type is specified like `as.xyz` or simply `xyz`
        # we'll try and interpret this as a function with or without the 
        # `as.` prefix.
        f = try(get(paste0("as.",type),mode = "function"),silent = TRUE)
        if (inherits(f,"try-error")) {
          f = try(get(type, mode = "function"),silent = TRUE)
        }
        if (inherits(f,"try-error")) {
          stop("The type `",type,"` is not defined (there is no `as.",type,"` function.)")
        }
      }
      
      if (!is.function(f)) {
        stop("The type `",type,"` did not resolve to a function.)")
      }
    }
    fs = c(fs,f)
  }
  return(function(x) tryCatch({
    for (f in fs) {
      x=f(x)
    }
    x
    },
    warning = function(e) stop("to a ",orig,": ",e$message),
    error = function(e) stop("to a ",orig,": ",e$message)
  ))
}

#' Define a conformance rule to match a factor with specific levels.
#' 
#' @param ... the levels (no quotes, backticks if required)
#' @param .drop should levels present in the data and not specified cause an error
#'  (FALSE the default) or be silently dropped to NA values (TRUE).
#'
#' @return a function that can check and convert input into the factor with specified
#'  levels. This will re-level factors with matching levels but in a different order.
#' @export
#' 
#' @concept rules
#'
#' @examples
#' f = enum(one,two,three)
#' f(c("three","two","one"))
#' f(factor(rep(1:3,5), labels = c("one","two","three")))
enum = function(..., .drop = FALSE) {
  lvls = unname(sapply(rlang::ensyms(...), rlang::as_label))
  if (length(lvls) == 0) return(.conv$factor)
  return(function(x) {
    if (is.null(x)) factor(NULL, levels = lvls)
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

#' Define a conformance rule to confirm that a numeric is in a set range
#' 
#' This is anticipated to be part of a `iface` rule e.g.
#' 
#' `iface(test_col = integer + in_range(-10,10) ~ "An integer from -10 to 10")`
#'
#' @param min the lower limit (inclusive)
#' @param max the upper limit (inclusive)
#'
#' @concept rules
#'
#' @return a function which checks the values and returns them if OK or throws 
#'   an error if not
#' @export
#'
#' @examples
#' in_range(0,100)(1:99)
#' try(in_range(0,10)(1:99))
in_range = function(min, max) {
  return(function(x) {
    x= as.numeric(x)
    if (any(stats::na.omit(x<min | x>max))) stop("values not in range: ",min,"-",max)
    x
  })
}

.conv = list(
  anything = function(x) {
    if (is.null(x)) character()
    return(x)
  },
  integer = function(x) {
    x = as.numeric(x)
    if (any(stats::na.omit(abs(x-round(x)) > .Machine$double.eps^0.5))) stop("not a true integer input") 
    return(as.integer(x))
  },
  positive_integer = function(x) {
    x = as.numeric(x)
    if (any(stats::na.omit(abs(x-round(x)) > .Machine$double.eps^0.5))) stop("not a true integer input") 
    if (any(x<0)) stop("positive integer smaller than zero")
    return(as.integer(x))
  },
  double = as.double,
  proportion = function(x) {
    x = as.double(x)
    if (any(x<0 | x>1)) stop("proportion outside of range 0 to 1")
    return(x)
  },
  positive_double = function(x) {
    x = as.double(x)
    if (any(x<0)) stop("positive double smaller than zero")
    return(x)
  },
  numeric = as.numeric,
  date = as.Date,
  logical = function(x) {
    x = as.numeric(x)
    if (any(stats::na.omit(!(x %in% c(0,1))))) stop("not a true logical input")
    return(as.logical(x))
  },
  factor = function(x) {
    if (is.null(x)) factor()
    if (is.factor(x)) return(x)
    forcats::as_factor(x)
  },
  character = as.character,
  group_unique = function(x) {
    if (is.null(x)) character()
    if (any(duplicated(x))) stop("non unique values detected")
    x
  },
  complete = function(x) {
    if (is.null(x)) stop("empty value cannot be complete")
    if (is.factor(x) & !(all(levels(x) %in% as.character(x)))) stop("not all factor levels represented")
    if (is.numeric(x) & !(all(.full_seq(x) %in% x))) stop("not all values present")
    x    
  }
)

#' @importFrom tidyr full_seq
#' @importFrom forcats as_factor
NULL

# we've created a version of full_seq that infers its own period.
# this is a shim to allow use of this if loaded or fall back to tidyr if
# not present.
.full_seq = function(x) {
  tryCatch(full_seq(x), error=function(e) full_seq(x,1))
}