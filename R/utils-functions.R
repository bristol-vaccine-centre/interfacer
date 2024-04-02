

.get_fn_name = function(fn) {
  if (is.null(fn)) return("<unknown>")
  fnenv= as.list(rlang::fn_env(fn))
  fnenv = fnenv[sapply(fnenv,is.function)]
  fnenv = lapply(fnenv, digest::digest)
  matches = sapply(fnenv, function(x) isTRUE(all.equal(x,digest::digest(fn))))
  if (any(matches)) return(paste0(names(fnenv)[matches],collapse = "/"))
  return("<unknown>")
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


# look for a block as the first argument of a function in the call stack
.search_call_stack = function(nframe = sys.nframe()-1, .class="roxy_block") {
  frame = sys.frame(nframe)
  first_arg_name = names(formals(sys.function(nframe)))[[1]]
  try({
    data = suppressWarnings(get(first_arg_name, envir=frame))
    if(inherits(data, .class)) return(data)
  },silent = TRUE)
  nframe = nframe-1
  if (nframe < 1) stop("no block found")
  .search_call_stack(nframe)
}