

#' Handle unexpected additional grouping structure
#' 
#' This function is designed to be used by a package author within an enclosing
#' function. The enclosing function is assumed to take as input a dataframe and
#' have an `iface` specified for that dataframe. 
#' 
#' This function detects when the grouping of the input has additional groups
#' over and above those in the specification and intercepts them, regrouping
#' the dataframe and applying `fn` group-wise using an equivalent of a 
#' `dplyr::group_modify`. The parameters provided to the enclosing function will be 
#' passed to `fn` and they should have compatible method signatures.
#'
#' @param df a dataframe from an enclosing function in which the grouping may or
#'   may not have been correctly supplied.
#' @param fn a function to call with the correctly grouped dataframe as specified
#'   by the `iface` in the enclosing function.
#' @param ... passed onto `iconvert` this could be used to supply 
#'   `.prune` parameters. triple dot parameters in the enclosing function will
#'   be separately handled and automatically passed to `fn` so in general should
#'   not be passed to `igroup_process` as an intermediary although it probably
#'   won't hurt. This behaviour is similar to `NextMethod` in S3 method
#'   dispatch.
#'
#' @concept interface
#'
#' @return the result of calling `fn(df, ...)` on each unexpected group
#' @export
#'
#' @examples
#' 
#' # This specification requires that the dataframe is grouped only by the color
#' # column
#' i_diamond_price = interfacer::iface(
#'   color = enum(`D`,`E`,`F`,`G`,`H`,`I`,`J`, .ordered=TRUE) ~ "the color column",
#'   price = integer ~ "the price column",
#'   .groups = ~ color
#' )
#' 
#' # An example function which would be exported in a package
#' ex_mean = function(df = i_diamond_price, extra_param = ".") {
#'   
#'   # When called with a dataframe with extra groups `igroup_process` will 
#'   # regroup the dataframe according to the structure 
#'   # defined for `i_diamond_price` and apply the inner function to each group
#'   # after first calling `ivalidate` on each group.
#'   
#'   igroup_process(df, 
#'     
#'     # the real work of this function is provided as an anonymous inner
#'     # function (but can be any other function e.g. package private function)
#'     # or a purrr style lambda.
#'     
#'     function(df, extra_param) {
#'       message(extra_param, appendLF = FALSE)
#'       return(df %>% dplyr::summarise(mean_price = mean(price)))
#'     }
#'     
#'   )
#' }
#' 
#' # The correctly grouped dataframe. The `ex_mean` function calculates the mean
#' # price for each `color` group.
#' ggplot2::diamonds %>% 
#'   dplyr::group_by(color) %>% 
#'   ex_mean(extra_param = "without additional groups...") %>% 
#'   dplyr::glimpse()
#'   
#' # If an additionally grouped dataframe is provided by the user. The `ex_mean` 
#' # function calculates the mean price for each `cut`,`clarity`, and `color` 
#' # combination.
#' 
#' ggplot2::diamonds %>% 
#'   dplyr::group_by(cut, color, clarity) %>% 
#'   ex_mean() %>% 
#'   dplyr::glimpse()
#'   
#' # The output of this is actually grouped by cut then clarity as
#' # color is consumed by the igroup_dispatch summarise.
#' 
igroup_process = function(df = NULL, fn, ...) {
  
  dispatch_fn = rlang::as_function(fn)
  # Get the spec from the enclosing function
  dname = tryCatch(rlang::as_label(rlang::ensym(df)), error = function(e) return(NA))
  fn = rlang::caller_fn()
  if (is.null(fn)) stop("`igroup_dispatch` must be called from within an enclosing function.", call. = FALSE)
  if (is.na(dname)) {
    df = .get_first_param_value()
    dname = .get_first_param_name()
  }
  fname = .get_fn_name(fn)
  spec = .get_spec(fn, dname)
  
  # TODO: the parameter name can be incorrect if the inner function is defined
  # using a different naming convention or more importantly if the variable is 
  # renamed:
  # function(x = iface(...)) {
  #   x2 = x
  #   igroup_process(x2, function(y) {...})
  # }
  # the logic to find dname finds the wrong name and cant match it to the spec. 
  # possibly because it is looking at the wrong function...
  
  exp_grps = .spec_grps(spec)
  obs_grps = dplyr::group_vars(df)
  
  # Dispatch environment:
  # have to dispatch using declared params from caller environment
  env = rlang::caller_env()
  # get any dots
  if ("..." %in% names(formals(fn))) {
    # evaluate `...` in the caller function environment.
    tmp = do.call(rlang::list2, list(as.symbol("...")), envir = env)
    params = c(as.list(env), tmp)
  } else {
    params = as.list(env)
  }
  
  additional_grps = setdiff(obs_grps, exp_grps)
  missing_grps = setdiff(exp_grps, obs_grps)
  
  if (length(missing_grps) > 0) {
    fmt_exp_grp = .none(exp_grps, collapse = ",", none = "%>% ungroup()", fmt = "%%>%% group_by(%s)")
    stop(
      sprintf("missing grouping in `%s` parameter of %s(...):\nmissing: %s\n", dname, fname, .none(missing_grps,"+")),
      sprintf("consider regrouping your data before calling function `%s`, e.g.:\n",fname),
      sprintf("`df %s %%>%% %s(...)`\n", fmt_exp_grp, fname),
      call. = FALSE
    )
  }
  
  
  
  if (length(additional_grps) == 0) {
    df = iconvert(df, spec, ...)
    if (!is.null(df)) {
      params[[dname]] = df
      return(do.call(dispatch_fn, params, envir = env))
    }
    stop("Could not validate dataframe input.", call. = FALSE)
  } else {
    # wrap the call to fn in a group_modify
    df = df %>% dplyr::group_by(dplyr::across(dplyr::all_of(additional_grps)))
    out = df %>% dplyr::group_modify(function(d,g,...) {
      # Fix any residual grouping issues:
      d = d %>% dplyr::group_by(dplyr::across(dplyr::all_of(exp_grps)))
      d = tryCatch(
        iconvert(d, spec, ...),
        error = function(e) {
          stop(
            "Could not validate dataframe input in group:\n",
            g %>% purrr::imap_chr(~ sprintf("%s=%s",.y,as.character(.x))) %>% paste0(collapse = "; "),
            "\n",
            e$message
          )
        }
      )
      if (!is.null(d)) {
        params[[dname]] = d
        return(do.call(dispatch_fn, params, envir = env))
      }
      stop("Could not validate dataframe input", call. = FALSE)
    })
    return(out)
  }
}