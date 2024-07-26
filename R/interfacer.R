

#' Perform interface checks on dataframe inputs using enclosing function formal
#'  parameter definitions
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
#' @return a dataframe based on `df` with validity checks passed and `.imap`
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
  fname = .get_fn_name(fn)
  
  .has_dots = "..." %in% names(formals(fn))
  # TODO: warn if spec names collides with formals.
  spec = .get_spec(fn, dname)
  
  if (is.iface(df)) {
    # We are validating something that should be a data frame but it is in fact
    # a iface spec. This means that no value has been supplied by the user and 
    # the iface has been returned by default.
    # We need to check is there is any ivalidate provided spec provided default?
    tmp = .spec_default(spec, .default, .env = rlang::caller_env(n = 2))
    # .spec_default checks validity of the .default parameter
    if (!is.null(tmp)) return(tmp)
    # There was no default
    stop("Missing parameter, ",dname," must be supplied.\n",format(spec), call. = FALSE)
  }
  
  if (!is.data.frame(df)) stop("Parameter `",dname,"` in function `",fname,"` was expected to be a dataframe, but it is a ",class(df)[[1]], call. = FALSE)
  
  # OK lets try make the dataframe conform to the spec.
  out = iconvert(df, spec, .imap, dname, fname, .has_dots, .prune)
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
#' @return a dataframe based on `df` with validity checks passed,
#'   data-types coerced, and correct grouping applied to conform to `iface`
#' @export
#' 
#' @concept interface 
#'
#' @examples
#' 
#' input = iface(col_in = integer ~ "an integer column" )
#' output = iface(col_out = integer ~ "an integer column" )
#' 
#' x = function(df = input, ...) {
#'   df = ivalidate(...)
#'   tmp = df %>% dplyr::rename(col_out = col_in)
#'   ireturn(tmp, output)
#' }
#' x(tibble::tibble(col_in = c(1,2,3)))
#' output
#' 
ireturn = function(df, iface, .prune=FALSE) {
  
  #TODO: bypass checks if the function is run in development
  
  spec = iface
  fn = rlang::caller_fn()
  fname = .get_fn_name(fn)
  out = df
  exp_grps = .spec_grps(spec)
  obs_grps = dplyr::group_vars(out)
  exp_cols = .spec_cols(spec)
  
  missing = dplyr::setdiff(exp_cols,colnames(out))
  if (length(missing) > 0) stop(
    sprintf("missing columns in the return value of %s(..):\n%s", fname, .none(missing,",")),
    call. = FALSE
  )
  # N.b. this implicitly checks all grouping columns that are mentioned are also
  # present.
  
  # Are there allowed additional groups?
  allowed_grps = setdiff(obs_grps, exp_grps)
  if (!.spec_allow_other(spec) && length(allowed_grps) > 0) {
    warning(sprintf(
      "unexpected additional groups in the return value of %s(...):\n%s", fname, .none(allowed_grps,"+")
    ))
    # we can pretend we have fixed them (but we haven't really):
    # this lets us carry on and check whether ignoring the additional groups
    # we have correct grouping
    obs_grps = intersect(obs_grps, exp_grps)
  } else {
    # no additional groups or 
    # they are allowed in the spec.
    # we update expected groups based on this knowledge
    exp_grps = c(allowed_grps, exp_grps)
  }
  
  # warn the developer that the grouping of the output is wrong.
  if(!identical(obs_grps,exp_grps)) {
    warning(
      sprintf("unexpected groups in the return value of %s(...):\nobserved: %s\nexpected: %s", 
              fname, 
              .none(obs_grps, "+"), 
              .spec_fmt_expected_groups(spec)
      )
    )
  }
  
  # Now force grouping to be as expected.
  # This will work as we have checked that all the columns exist.
  # exp_grps is updated to include allowed grps.
  # In theory this could lead to reordering of columns.
  out = out  %>% dplyr::group_by(dplyr::across(tidyselect::all_of(exp_grps)))
  
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(unique(c(exp_cols,allowed_grps))))
  }
  out = out %>% .coerce(spec, fname)
  return(out)
}
  









#' Convert a dataframe to a format compatible with an interface specification
#' 
#' This function is called by `ivalidate` and is not generally intended to be
#' used directly by the end user. It may be helpful in debugging during package 
#' development to interactive test a `iface` spec. `iconvert` is an interactive 
#' version of `ivalidate`.
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
#' @param .env internal use only
#'
#' @return the input dataframe coerced to be conformant to the `iface`
#'   specification, or an informative error is thrown.
#' @export
#'
#' @examples
#' i_diamonds = iface( 
#'   color = enum(D,E,F,G,H,I,J,extra) ~ "the colour", 
#'   price = integer ~ "the price"
#' )
#' 
#' iconvert(ggplot2::diamonds, i_diamonds,.prune = TRUE)
#' 
#' 
iconvert = function(df, iface, .imap = interfacer::imapper(), .dname="<unknown>", .fname="<unknown>", .has_dots = TRUE, .prune = FALSE, .env = rlang::current_env()) {
  
  #TODO cache this?
  
  #TODO: consider deprecating `.imap`
  # apply any imap 
  dots = .imap
  dots = dots[names(dots) %in% .spec_cols(iface)]
  out = df %>% dplyr::mutate(!!!dots)
  
  spec = iface
  if (!is.iface(spec)) stop("iface must be a `interfacer::iface(...) specification", call. = FALSE)
  
  exp_grps = .spec_grps(spec)
  obs_grps = dplyr::group_vars(out)
  exp_cols = .spec_cols(spec)
  
  # Check for missingness
  missing = dplyr::setdiff(exp_cols,colnames(out))
  if (length(missing) > 0) {
    if (.dname == "nested") stop("missing columns: ",.none(missing,","), call. = FALSE)
    stop(
      sprintf("missing columns in the `%s` parameter of `%s(...)`.\nmissing: %s\n", .dname, .fname, .none(missing,",")),
      sprintf("consider renaming / creating missing columns before calling `%s(...)`\n", .fname),
      # if (.has_dots) sprintf("or by adding `.imap = interfacer::imapper(%s)` to your function call.\n", .none(missing, ", ", fmt_item="`%s`=...")) else "",
      call. = FALSE
    )
  }
  # N.b. this implicitly checks all grouping columns that are mentioned are also
  # present.
  
  # Are there allowed additional groups?
  allowed_grps = setdiff(obs_grps, exp_grps)
  if (!.spec_allow_other(spec) && length(allowed_grps) > 0) {
    if (.dname == "nested") stop("unexpected groups: ",.none(allowed_grps,","), call. = FALSE)
    fmt_exp_grp = .none(exp_grps, collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
    fmt_diff_grp = .none(allowed_grps, collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
    stop(
      sprintf("unexpected additional groups in `%s` parameter of `%s(...)`\nadditional: %s\n", .dname, .fname, .none(allowed_grps,"+")),
      sprintf("consider regrouping your data before calling function `%s(...)`, e.g.:\n",.fname),
      sprintf("`df %s %%>%% %s(...)`\n", fmt_exp_grp, .fname),
      sprintf("or calling function `%s(...)` using a group_modify, e.g.:\n",.fname),
      sprintf("`df %s %%>%% group_modify(%s, ...)`", fmt_diff_grp, .fname),
      call. = FALSE
    )
  }
  
  missing_grps = setdiff(exp_grps, obs_grps)
  if (length(missing_grps) > 0) {
    if (.dname == "nested") stop("missing groups: ",.none(missing_grps,","), call. = FALSE)
    fmt_exp_grp = .none(c(allowed_grps,exp_grps), collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
    stop(
      sprintf("missing grouping in `%s` parameter of `%s(...)`:\nmissing: %s\n", .dname, .fname, .none(missing_grps,"+")),
      sprintf("consider regrouping your data before calling function `%s(...)`, e.g.:\n",.fname),
      sprintf("`df %s %%>%% %s(...)`\n", fmt_exp_grp, .fname),
      call. = FALSE
    )
  }
  
  if (.prune) {
    out = out %>% dplyr::select(tidyselect::all_of(unique(c(allowed_grps,exp_cols))))
  }
  
  # TODO: propagate .prune to nested dataframes...
  
  out = out %>% .coerce(spec,.fname,.dname, .env)
  return(out)
}

.strict = function(fn,name,.fname,.dname) {
  return(function(x) {
    tryCatch(
      fn(x),
      error = function(e) {
        if (.dname == "nested") stop("incorrect type: ",name, call. = FALSE)
        stop(
        if (!is.null(.dname)) {
          sprintf("input column `%s` in function parameter `%s(%s = ?)` cannot be coerced ",name,.fname,.dname)
        } else {
          sprintf("output column `%s` in return value from `%s(...)` cannot be coerced ",name,.fname)
        },
        e$message,
        call. = FALSE) 
      }
    )       
  })
}

# use a spec to coerce a dataframe to the correct value
# scoping of checks if complex 
# prior to this being called grouping has been checked and
# if needs be enforced - i.e. the grouping is correct.
.coerce = function(grp_df, spec, .fname, .dname="<unknown>", .env = rlang::caller_env()) {
  
  out = grp_df
  exp_grps = .spec_grps(spec,sym = TRUE)
  obs_grps = dplyr::groups(out)
  exp_cols = .spec_cols(spec,sym = TRUE)
  add_grps = setdiff(obs_grps,exp_grps)
  data_cols = setdiff(exp_cols,obs_grps)
  
  # add_grps - every check has to operate groupwise within these.
  # any checks on these columns operate ungrouped.
  # 
  # exp_grps - we add in each group one at a time.
  # check conformance for next item of exp_grps (if it exists)
  # group incrementally
  # 
  # then check remaining data columns when fully grouped.
  # 
  # so we can have
  # grp1 ~ factor + complete <-- complete within <any>
  # grp2 ~ factor + complete <-- complete within <any>+grp1
  # data ~ integer + group_unique
  # .groups = ~ . + grp1 + grp2
  
  out = out %>% dplyr::ungroup()
  
  issues = character()
  
  for (add_grp in add_grps) {
    # These are additionally grouped columns. Checks on them are 
    # scoped to the ungrouped data frame.
    if (.spec_has_rule(spec, rlang::as_label(add_grp))) {
      type = .spec_type_of(spec, rlang::as_label(add_grp))
      asfn = .get_conv(type, .fname, .dname, .env)
      asfn2 = .strict(asfn, rlang::as_label(add_grp), .fname, .dname)
      out = tryCatch({
        out %>% dplyr::mutate(!!add_grp := asfn2(!!add_grp))
      }, error = function(e) {
        issues <<- c(issues,e$message)
        return(out)
      })
    }
    
    out = out %>% dplyr::group_by(!!add_grp, .add = TRUE)
  }
  
  for (new_grp in exp_grps) {
    
    # these are the mandatory groups 
    # checks on these are run in the order the columns are defined
    # and are scoped to the grouping up to that point.
    
    if (.spec_has_rule(spec, rlang::as_label(new_grp))) {
      type = .spec_type_of(spec, rlang::as_label(new_grp))
      asfn = .get_conv(type, .fname, .dname, .env)
      asfn2 = .strict(asfn, rlang::as_label(new_grp), .fname, .dname)
      out = tryCatch({
        out %>% dplyr::mutate(!!new_grp := asfn2(!!new_grp))
      }, error = function(e) {
        issues <<- c(issues,e$message)
        return(out)
      })
    }
    out = out %>% dplyr::group_by(!!new_grp,.add = TRUE)
  }
  
  for (data_col in data_cols) {
    if (.spec_has_rule(spec, rlang::as_label(data_col))) {
      type = .spec_type_of(spec, rlang::as_label(data_col))
      asfn = .get_conv(type, .fname, .dname, .env)
      asfn2 = .strict(asfn, rlang::as_label(data_col), .fname, .dname)
      out = tryCatch({
        out %>% dplyr::mutate(!!data_col := asfn2(!!data_col))
      }, error = function(e) {
        issues <<- c(issues, e$parent$message)
        return(out)
      })
    }
  }
  
  if (length(issues) > 0) stop(paste0(issues,collapse="\n"),call. = FALSE)
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

