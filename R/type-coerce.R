
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
    # Unique key columns have already been checked. They don't have a 
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
    error = function(e) stop("to a ",orig,": ",e$message, call. = FALSE),
    warning = function(e) stop("to a ",orig,": ",e$message, call. = FALSE)
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
    if (!is.list(x)) stop("not a list column", call. = FALSE)
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
            "nested dataframe problem - ",e$message, call. = FALSE) #,
          #envir = .env
        #)
      )
      return(out)
    }
  )
}

.stack_eval = function(expr) {
  g = NULL
  for (x in sys.frames()) { 
    f = tryCatch(eval(str2lang(expr),envir = x),error = function(e) NULL)
    if (!is.null(f) && is.function(f)) return(f)
    if (is.null(g)) g = f
  }
  if (is.null(g)) return(NULL)
  return(paste0("`",expr,"` must evaluate to a function. it is a ",class(g)[[1]]))
}

.stack_eval_iface = function(expr) {
  g = NULL
  for (x in sys.frames()) { 
    f = tryCatch(eval(str2lang(expr),envir = x),error = function(e) NULL)
    if (!is.null(f) && is.iface(f)) return(f)
    if (is.null(g)) g = f
  }
  if (is.null(g)) return(NULL)
  return(paste0("`",expr,"` must evaluate to a iface. it is a ",class(g)[[1]]))
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
  pkg = stringr::str_extract(type, "^([^:]+::)",group = 1)
  if (is.na(pkg)) pkg=""
  type = stringr::str_extract(type, "^([^:]+::)?([^\\(]+)",group = 2)
  # if not param will be NA.
  
  # first off can we interpret `type` as a function like
  # type.integer as defined in this package?
  iface_type = sprintf("%stype.%s", pkg, type)
  f = .stack_eval(iface_type)
  if (!is.null(f) && is.function(f)) {
    # some of these might be parameterised functions that return functions, 
    # others are not. Either way it should be possible to attempt to evaluate
    # reconstruct expression (t)
    if (!is.na(param)) iface_type = sprintf("%s(%s)",iface_type,param)
    
    f = try(eval(str2lang(iface_type), envir = .env),silent = TRUE)
    # any error here is an error in the interface spec and needs to be thrown
    # to the developer. We are not testing the data at this point, just finding
    # the correct type coercion function. Any issue here should surface in package
    # development
    if (inherits(f,"try-error")) stop("Couldn't evaluate the type: ",type,"\nBecause of ",attr(f,"condition")$message, call. = FALSE)
    return(f)
  }
  
  if (type == "list") {
    # a list of things
    return(.list_test(param, .fname, .dname, .env))
  }
  
  i = .stack_eval_iface(expr)
  if (!is.null(i) && is.iface(i)) {
    # the type is an interface spec - usually nested in a list column, or
    # can be a tibble column in a tibble.
    return(.nested_iface(i, .fname, .dname, .env))
  }
  
  # So the type is something unusual. maybe something like `as.POSIXct`
  # or even just `POSIXct`. But it isn't from interfacer.
  # we will just try and evaluate the expr with 
  # `as.` as a prefix, and then as-is if neither works we halt and catch fire.
  
  expr2 = sprintf("%sas.%s",pkg, type)
  f = .stack_eval(expr2)
  if (!is.null(f) && is.function(f)) return(f)
  
  f = .stack_eval(expr)
  if (!is.null(f) && is.function(f)) return(f)
  
  # Stack eval returns an error message as a character
  if (!is.null(f)) stop(f)
  
  # the type specification has something odd in it and we cant figure it out
  stop("Couldn't evaluate type specification:\n`",
       expr,"` or `", expr2, 
       call. = FALSE
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
      if(!all(stats::na.omit(x) %in% lvls) && !.drop) stop("values found not in allowed levels", call. = FALSE)
      return(factor(x, lvls, ordered = .ordered))
    }
    stop("enum not a factor or a character", call. = FALSE)
  })
}

#' Define a conformance rule to confirm that a numeric is in a set range
#' 
#' This is anticipated to be part of a `iface` rule e.g.
#' 
#' `iface(test_col = integer + in_range(-10,10) ~ "An integer from -10 to 10")`
#'
#' @param min the lower limit
#' @param max the upper limit
#' @param include.min is lower limit open (default TRUE)
#' @param include.max is upper limit open (default TRUE)
#'
#' @concept rules
#'
#' @return a function which checks the values and returns them if OK or throws 
#'   an error if not
#' @export
#'
#' @examples
#' type.in_range(0,10,TRUE,TRUE)(0:10)
#' try(type.in_range(0,10,TRUE,FALSE)(0:10))
#' try(type.in_range(0,10,FALSE)(0:10))
#' type.in_range(0,10,FALSE,TRUE)(1:10)
#' type.in_range(0,10,TRUE,FALSE)(0:9)
#' type.in_range(0,Inf,FALSE,FALSE)(1:9)
#' try(type.in_range(0,10)(1:99))
type.in_range = function(min, max, include.min = TRUE, include.max = TRUE) {
  if (!is.numeric(min) || !is.numeric(max) || length(min) != 1 || length(max) != 1 || min >= max )
    stop("in_range: `min` and `max` must be single numbers and min < max", call. = FALSE)
  lbl = paste0(
    format(min),
    if (include.min) " \u2264 " else " < ",
    "x",
    if (include.max) " \u2264 " else " < ",
    format(max)
  )
  return(function(x) {
    x= as.numeric(x)
    if (include.min) {
      # LHS open
      if (any(stats::na.omit(x<min))) stop("values not in range: ",lbl, call. = FALSE)
    } else {
      # LHS closed
      if (any(stats::na.omit(x<=min))) stop("values not in range: ",lbl, call. = FALSE)
    }
    if (include.max) {
      # RHS open
      if (any(stats::na.omit(x>max))) stop("values not in range: ",lbl, call. = FALSE)
    } else {
      # RHS closed
      if (any(stats::na.omit(x>=max))) stop("values not in range: ",lbl, call. = FALSE)
    }
    x
  })
}


#' Coerce to an unspecified type
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
    x = tryCatch(
      as.numeric(x), 
      error = function(e) stop("error casting to integer: ",e$message, call. = FALSE),
      warning = function(w) stop("non numeric format", call. = FALSE)
    )
    if (any(stats::na.omit(abs(x-round(x)) > .Machine$double.eps^0.5))) stop("rounding detected", call. = FALSE) 
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
    x = type.integer(x)
    if (!all(stats::na.omit(x >= 0))) stop("negative number detected where none allowed", call. = FALSE)
    return(x)
}

#' Coerce to a double.
#'
#' @param x any vector
#' @return the input as a double, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.double = function(x) {
    x = tryCatch(
      as.double(x), 
      error = function(e) stop("error casting to numeric: ",e$message, call. = FALSE),
      warning = function(w) stop("non numeric format", call. = FALSE)
    )
    return(x)
  }

#' Coerce to a number between 0 and 1
#'
#' @inheritParams base::double
#' @return the input as a number from 0 to 1, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.proportion = function(x) {
    x = as.double(x)
    if (!all(stats::na.omit(x >= 0 & x <= 1))) stop("proportion outside of range 0 to 1", call. = FALSE)
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
    if (!all(stats::na.omit(x >= 0))) stop("negative number detected where none allowed", call. = FALSE)
    return(x)
}

#' Coerce to a numeric.
#'
#' @param x any vector
#' @return the input as a numeric, error if this would involve data loss.
#' 
#' @concept rules
#' @export
type.numeric = function(x) {
  x = tryCatch(
    as.numeric(x), 
    error = function(e) stop("error casting to numeric: ",e$message, call. = FALSE),
    warning = function(w) stop("non numeric format", call. = FALSE)
  )
  return(x)
}

#' Coerce to a Date.
#'
#' @inheritParams base::as.Date
#' @concept rules
#' @return the input as a `date` vector, error if this would involve data loss.
#' @export
type.date = function(x,...) {
  x = tryCatch(
    as.Date(x, ...), 
    error = function(e) stop("error casting to date: ",e$message, call. = FALSE),
    warning = function(w) stop("non compatible date format", call. = FALSE)
  )
  return(x)
}

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
    if (is.numeric(x))
      if (!all(stats::na.omit(as.numeric(x)) %in% c(0,1))) stop("rounding deteced", call. = FALSE)  
    tmp = as.logical(x)
    if (any(is.na(tmp) & !is.na(x))) stop("not T/F input", call. = FALSE)
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

#' Coerce to a unique value within the current grouping structure.
#'
#' @param x any vector
#'
#' @return the input or error if any of x is not unique.
#' 
#' @concept rules
#' @export
type.group_unique = function(x) {
    if (is.null(x)) return(character())
    if (!all(!duplicated(stats::na.omit(x)))) stop("values are not unique within each group; check grouping is correct", call. = FALSE)
    x
}

#' A globally unique ids.
#'
#' @param x any vector
#'
#' @return the input.
#' 
#' @concept rules
#' @export
type.unique_id = function(x) {
  # Uniqueness of keys is checked prior to type coercion. This function is here
  # simply to make sure that the other rules can be applied at the same time.
  x
}

#' Coerce to a complete set of values.
#' 
#' This test checks either for factors that all factor levels are present in the
#' input, or for numerics if the sequence from minimum to maximum by the
#' smallest difference are not all (approximately) present. Empty values are
#' ignored.
#'
#' @param x any vector, factor or numeric
#'
#' @return the input or error if not complete
#' 
#' @concept rules
#' 
#' @importFrom forcats as_factor
#' @export
type.complete = function(x) {
  if (is.null(x) || length(x)==0) return(x)
  if (all(is.na(x))) stop("only missing values found when checking for completeness", call. = FALSE)
  
  if (is.factor(x)) {
    if (!(all(levels(x) %in% as.character(x)))) stop("not all factor levels represented", call. = FALSE)
    else return(x)
  } else if (is.numeric(x)) {
    tmp = sort(unique(as.numeric(x)))
    comp = seq(min(tmp,na.rm = TRUE), max(tmp,na.rm = TRUE), .step(tmp))
    if (length(tmp) != length(comp)) stop("full range of numeric values not present", call. = FALSE)
    if (any(abs(tmp-comp) > .Machine$double.eps ^ 0.5)) stop("full range of numeric values not present", call. = FALSE)
  } else {
    stop("`complete` constraint used with unsupported column type: ",class(x))
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
  if (length(value) != 1 ) stop("default values must be length 1 (or wrapped in a list)", call. = FALSE)
  return(function(x) {
    if (is.null(x)) return(utils::head(x,0))
    if (is.factor(x)) return(forcats::fct_na_value_to_level(x, level = as.character(value)))
    return(ifelse(is.na(x), value, x))
  })
}

#' Check for missing values
#' 
#' Any NA values will cause failure of validation. 
#'
#' @param x any vector, factor or numeric
#' @return the input if no missing values detected, otherwise an error
#' @export
#'
#' @concept rules
type.not_missing = function(x) {
  if (is.null(x)) return(character())
  if (any(is.na(x))) stop("missing values where none allowed", call. = FALSE)
  return(x)
}

#' Check for non-finite values
#' 
#' Any non finite values will cause failure of validation. 
#'
#' @param x any vector that can be coerced to numeric
#' @return the input coerced to a numeric value, or an error if any non-finite
#'   values detected
#' @export
#'
#' @concept rules
type.finite = function(x) {
  if (is.null(x)) return(numeric())
  x = as.numeric(x)
  if (any(!is.finite(x))) stop("non-finite values where none allowed", call. = FALSE)
  return(x)
}


#' Check for a given class
#' 
#' Any values of the wrong class will cause failure of validation. This is 
#' particularly useful for custom vectors of for list types (e.g. `list(of_type(lm))`)
#'
#' @param type the class of the type we are checking as a symbol
#' @param .not_null are NULL values allowed (for list column entries only)
#' @return a function that can check the input is of the correct type.
#' @export
#'
#' @concept rules
type.of_type = function(type, .not_null = FALSE) {
  type = rlang::as_label(rlang::ensym(type))
  return(
    function(x) {
      if (is.null(x)) {
        if (.not_null) stop(sprintf("NULL values not allowed"), call. = FALSE)
        return(NULL)
      }
      if (!inherits(x,type)) stop(sprintf("incorrect type, should be a `%s`",type), call. = FALSE)
      return(x)
    }
  )
}


# The smallest interval in a vector
.step = function(x) {
  y = sort(unique(x))
  dy = stats::na.omit(y[-1]-utils::head(y,-1))
  return(min(dy))
}
