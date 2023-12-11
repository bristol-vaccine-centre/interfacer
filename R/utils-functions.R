

.get_fn_name = function(fn) {
  if (is.null(fn)) return("<unknown>")
  fnenv= as.list(rlang::fn_env(fn))
  matches = sapply(fnenv, function(x) isTRUE(all.equal(x,fn)))
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
