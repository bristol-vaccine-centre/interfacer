
# Conversion selection

# the only function from this file that gets called directly is this one:

# f = .get_conv("enum(x,y,z)")
# f(c("x","y"))
# parses the type and returns a function that can validate the input.
# g = .get_conv("as.list")
# g(c(1,2,3))
# f2 = .get_conv("group_unique + integer")
# try(f2(c(1:10,1:10)))
# f3 = .get_conv("list(integer)")
# f3(list(c(1,2,3), c(4,5)))
.get_conv = function(type, .fname, .dname, .env=rlang::caller_env()) {
  orig = type
  types = stringr::str_split(type, "\\s?\\+\\s?")[[1]]
  fs = list()
  
  # get a list of functions that match the string type
  for (type in types) {
    f = .find_function(type, .fname, .dname, .env)
    fs = c(fs,f)
  }
  
  # return the functional composition of all the functions
  # errors and warning checked and thrown.
  return(function(x) tryCatch({
      for (f in fs) {
        x=f(x)
      }
      x
    },
    # must be this way round otherwise warning is caught twice.
    error = function(e) stop("to a ",orig,": ",e$message),
    warning = function(e) stop("to a ",orig,": ",e$message)
  ))
}


# Handle a list as a type.
# this returns a function that when called
# checks and coerces the individual list elements to the 
# type specified in `type`
.list_test = function(type, .fname, .dname, .env=rlang::caller_env()) {
  f = .find_function(type, .fname, .dname, .env)
  return(function(x) {
    if (is.null(x)) return(list())
    if (!is.list(x)) stop("not a list column")
    lapply(x, f)
  })
}

# Handle a nested iface as a type
# this returns a function that when called
# checks and coerces the dataframe to the type 
# specified in iface
.nested_iface = function(iface, .fname, .dname, .env=rlang::caller_env()) {
  return(
    function(x) {
      if (is.null(x)) return(iproto(iface))
      out = tryCatch(
        # eval(
          iconvert(x, 
            iface, 
            .fname=.fname, 
            .dname="nested",
            .prune = FALSE,
            .env = .env),
          error = function(e) stop(
            "nested dataframe problem - ",e$message) #,
          #envir = .env
        #)
      )
      return(out)
    }
  )
}

# find a (single) function for a single type spec
# within function .fname and for parameter .dname
.find_function = function(type, .fname, .dname, .env=rlang::caller_env()) {
  
  # Already is a function
  if (is.function(type)) return(type)
  
  # there is a possibility that type is a pseudo function or
  # a list specification:
  expr = type
  param = stringr::str_extract(type, "[^\\(]\\((.*)\\)",group = 1)
  type = stringr::str_extract(type, "^([^\\(]+)",group = 1)
  # if not param will be NA.
  
  # first off can we interpret `type` as a function like
  # type.integer as defined in this package?
  iface_type = sprintf("type.%s", tolower(type))
  if (exists(iface_type,mode = "function",envir = .env)) {
    # some of these might be parameterised functions that return functions, 
    # others are not. Either way it should be possible to attempt to evaluate
    # reconstruct expression (t)
    if (!is.na(param)) iface_type = sprintf("%s(%s)",iface_type,param)
    
    f = try(eval(str2lang(iface_type), envir = .env),silent = TRUE)
    # any error here is an error in the interface spec and needs to be thrown
    # to the developer. We are not testing the data at this point, just finding
    # the correct type coercion function. Any issue here should surface in package
    # development
    if (inherits(f,"try-error")) stop("Couldn't evaluate the type: ",type,"\nBecause of ",attr(f,"condition")$message)
    return(f)
  }
  
  if (tolower(type) == "list") {
    # a list of things
    return(.list_test(param, .fname, .dname, .env))
  }
   
  if (exists(type, envir = .env) && is.iface(get(type, envir = .env))) {
    # the type is an interface spec - usually nested in a list column, or
    # can be a tibble column in a tibble.
    return(.nested_iface(get(type, envir = .env), .fname, .dname, .env))
  }
  
  # So the type is something unusual. maybe something like `as.POSIXct`
  # or even just `POSIXct`. But it isn't from interfacer.
  # we will just try and evaluate the expr with 
  # `as.` as a prefix, and then as-is if neither works we halt and catch fire.
  
  expr2 = sprintf("as.%s",expr)
  f = try(eval(str2lang(expr2), envir = .env),silent = TRUE)
  if (!inherits(f,"try-error") && is.function(f)) return(f)
  mess2 = attr(f,"condition")$message
  
  f = try(eval(str2lang(expr), envir = .env),silent = TRUE)
  if (!inherits(f,"try-error") && is.function(f)) return(f)
  mess1 = attr(f,"condition")$message
  
  if (!inherits(f,"try-error")) stop("`",expr,"` must evaluate to a function. it is a ",class(f)[[1]])
  
  # the type specification has something odd in it and we cant figure it out
  stop("Couldn't evaluate type specification:\n`",
       expr,"`: ",mess1,"\n`", 
       expr2,"`: ",mess2,""
  )
  
}

## Converters ----




#' Define a conformance rule to match a factor with specific levels.
#' 
#' @param ... the levels (no quotes, backticks if required)
#' @param .drop should levels present in the data and not specified cause an error
#'  (FALSE the default) or be silently dropped to NA values (TRUE).
#' @param .ordered must the factor be ordered
#'
#' @return a function that can check and convert input into the factor with specified
#'  levels. This will re-level factors with matching levels but in a different order.
#' @export
#' 
#' @concept rules
#'
#' @examples
#' f = type.enum(one,two,three)
#' f(c("three","two","one"))
#' f(factor(rep(1:3,5), labels = c("one","two","three")))
type.enum = function(..., .drop = FALSE, .ordered = FALSE) {
  lvls = unname(sapply(rlang::ensyms(...), rlang::as_label))
  if (length(lvls) == 0) return(type.factor)
  return(function(x) {
    if (is.null(x)) return(factor(NULL, levels = lvls, ordered = .ordered))
    if (is.factor(x)) {
      if (identical(levels(x), lvls)) return(x)
      x = as.character(x)
    }
    if (is.character(x) || all(is.na(x))) {
      if(!all(stats::na.omit(x) %in% lvls) && !.drop) stop("values present not in levels")
      return(factor(x, lvls, ordered = .ordered))
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
#' type.in_range(0,100)(1:99)
#' try(type.in_range(0,10)(1:99))
type.in_range = function(min, max) {
  if (!is.numeric(min) || !is.numeric(max) || length(min) != 1 || length(max) != 1 || min >= max )
    stop("in_range: `min` and `max` must be single numbers and min < max")
  return(function(x) {
    x= as.numeric(x)
    if (any(stats::na.omit(x<min | x>max))) stop("values not in range: ",min,"-",max)
    x
  })
}


#' Coerce a unspecified type
#'
#' @param x any vector
#'
#' @return the input (unless x is `NULL` in which case a `character()`)
#' 
#' @concept rules
#' @export
type.anything = function(x) {
    if (is.null(x)) return(character())
    return(x)
}

#' Coerce to integer
#'
#' @param x any vector
#'
#' @return the input as an integer, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.integer = function(x) {
    x = as.numeric(x)
    if (!all(stats::na.omit(abs(x-round(x)) < .Machine$double.eps^0.5))) stop("not a true integer input") 
    return(as.integer(x))
}

#' Coerce to a positive integer.
#'
#' @param x any vector
#'
#' @return the input as a positive integer, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.positive_integer = function(x) {
    x = as.numeric(x)
    if (!all(stats::na.omit(abs(x-round(x)) < .Machine$double.eps^0.5))) stop("not a true integer input") 
    if (!all(stats::na.omit(x >= 0))) stop("positive integer smaller than zero")
    return(as.integer(x))
}

#' Coerce to a double.
#'
#' @return the input as a double, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.double = as.double

#' Coerce to a number between 0 and 1
#'
#' @inheritParams base::double
#' @return the input as a number from 0 to 1, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.proportion = function(x) {
    x = as.double(x)
    if (!all(stats::na.omit(x >= 0 & x <= 1))) stop("proportion outside of range 0 to 1")
    return(x)
}

#' Coerce to a positive double.
#'
#' @inheritParams base::double
#' @return the input as a positive double, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.positive_double = function(x) {
    x = as.double(x)
    if (!all(stats::na.omit(x >= 0))) stop("positive double smaller than zero")
    return(x)
}

#' Coerce to a numeric.
#'
#' @return the input as a numeric, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.numeric = as.numeric

#' Coerce to a Date.
#'
#' @inheritParams base::as.Date
#' @concept rules
#' @export
type.date = as.Date

#' Coerce to a logical
#'
#' @param x any vector
#'
#' @return the input as a logical, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.logical = function(x) {
    if(is.null(x)) return(logical())
    x = as.numeric(x)
    if (!all(stats::na.omit(x) %in% c(0,1))) stop("not a true logical input")
    return(as.logical(x))
}

#' Coerce to a factor.
#'
#' @param x any vector
#'
#' @return the input as a factor, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.factor = function(x) {
    if (is.null(x)) return(factor())
    if (is.factor(x)) return(x)
    forcats::as_factor(x)
}

#' Coerce to a character.
#'
#' @return the input as a character.
#' 
#' @concept rules
#' @export
type.character = as.character

#' Coerce to a unique value.
#'
#' @param x any vector
#'
#' @return the input, error if any of x is not unique.
#' 
#' @concept rules
#' @export
type.group_unique = function(x) {
    if (is.null(x)) return(character())
    if (!all(!duplicated(stats::na.omit(x)))) stop("non unique values detected")
    x
}

#' Coerce to a complete set of values.
#'
#' @param x any vector, factor or numeric
#'
#' @return the input, error if not all factor levels are present, of for numerics if
#'   the sequence from minimum to maximum by the smallest difference are not all
#'   approximately present.
#' 
#' @concept rules
#' 
#' @importFrom forcats as_factor
#' @export
type.complete = function(x) {
  if (is.null(x) || all(is.na(x))) stop("empty value cannot be complete")
  if (is.factor(x) & !(all(levels(x) %in% as.character(x)))) stop("not all factor levels represented")
  if (is.numeric(x)) {
    tmp = sort(unique(as.numeric(x)))
    comp = seq(min(tmp,na.rm = TRUE), max(tmp,na.rm = TRUE), .step(tmp))
    if (length(tmp) != length(comp)) stop("not all values present")
    if (any(abs(tmp-comp) > .Machine$double.eps ^ 0.5)) stop("not all values present")
  }
  x    
}



#' Set a default value for a column
#' 
#' Any NA values will be replaced by this value. N.b. default values must be 
#' provided before any other rules if the validation is not to fail.
#'
#' @param value a length one item of the correct type.
#'
#' @return a validation function that switches NAs for default values
#' @export
#'
#' @concept rules
type.default = function(value) {
  if (length(value) != 1 ) stop("default values must be length 1 (or wrapped in a list)")
  return(function(x) {
    if (is.null(x)) return(utils::head(x,0))
    if (is.factor(x)) return(forcats::fct_na_value_to_level(x, level = as.character(value)))
    return(ifelse(is.na(x), value, x))
  })
}


# The smallest interval in a vector
.step = function(x) {
  y = sort(unique(x))
  dy = stats::na.omit(y[-1]-utils::head(y,-1))
  return(min(dy))
}
