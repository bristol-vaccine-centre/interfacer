
# TODO: nested list columns and recursion.

#' Create an `iface` interface from an example dataframe
#'
#' Copies the `iface` specification to the clipboard
#'
#' @param df a prototype dataframe
#' @param df_name an optional name for the parameter
#'
#' @concept document
#'
#' @return nothing
#' @export
#'
#' @examples
#' if(FALSE) iclip(iris)
iclip = function(df, df_name=deparse(substitute(df))) {
  
  tmp = .infer_nested_structure(df, paste0("i_",df_name))
  
  tmp2 = sapply(names(tmp), function(nm) {
    sprintf("%s = %s",nm,.generate_code(tmp[[nm]]))
  })
  
  tmp2 = paste0(tmp2,collapse = "\n\n")
  
  message(df_name, " specification copied to clipboard... Ctrl-V to paste.")
  clipr::write_clip(tmp2)
}

.generate_code = function(spec, default_variable = NULL) {
  if (.spec_allow_other(spec)) {
    fmt_grps = .none(.spec_grps(spec), collapse = " + ",fmt = ".groups = ~ . + %s",none = ".groups = NULL")
  } else {
    fmt_grps = .none(.spec_grps(spec), collapse = " + ",fmt = ".groups = ~ %s",none = ".groups = FALSE")
  }
  
  tmp = sprintf("interfacer::iface(\n\t%s\n)",
      paste0(
        c(
          sprintf("%s = %s ~ \"%s\"", spec$name, spec$type, spec$doc),
          fmt_grps,
          if (!is.null(default_variable)) sprintf(".default = %s",default_variable) else NULL
        ),
        collapse = ",\n\t"
      )
  )
  return(tmp)
}


.infer_nested_structure = function(df, name=paste0("i_",deparse(substitute(df))), allow_other=TRUE, use_as_default = FALSE) {
  
  out = list(
    .infer_structure(df, allow_other, use_as_default) 
  )
  # level 0 name may be defined, sub levels the names come from the columns
  # vai the lapply
  if (!is.null(name)) names(out) = name
  
  # recursive for list columns of dataframes
  # relies on all items in column matching the first.
  tmp = lapply(df, function(col) {
    if (is.list(col) && is.data.frame(col[[1]])) {
      return(.infer_nested_structure(col[[1]], NULL, allow_other, FALSE))
    } else {
      return(NULL)
    }
  })
  names(tmp) = paste0("i_", names(tmp))
  
  out = c(out,unlist(tmp,recursive = FALSE))
  return(out)
}

.infer_structure = function(df, allow_other=TRUE, use_as_default = FALSE) {
  
  # TODO: list columns of dataframes.
  # This maybe needs to return a list of ifaces rather than just one.
  
  names = colnames(df)
  types = unname(sapply(names, function(n) {.infer_type(df[[n]],n)}))
  docs = sprintf("the %s column",names)
  tmp = structure(
    tibble::tibble(
      name = names,
      type = types,
      doc = docs
    ),
    groups = dplyr::group_vars(df),
    allow_other = allow_other,
    default = if(use_as_default) df else NULL,
    class=c("iface",class(tibble::tibble())))
  return(tmp)
}

.infer_type = function(x, name) {
  if (all(is.na(x))) return("anything")
  if (inherits(x,"factor")) {
    if (is.ordered(x)) {
      return(sprintf("enum(%s, .ordered=TRUE)", paste0("`",levels(x),"`",collapse=",")))
    } else {
      return(sprintf("enum(%s)", paste0("`",levels(x),"`",collapse=",")))
    }
  }
  if (is.data.frame(x)) return(sprintf("i_%s",name))
  if (is.list(x)) return(sprintf("list(%s)",.infer_type(x[[1]], name)))
  tmp = class(x)[[1]]
  if (tolower(tmp) %in% .converter_names()) return(tolower(tmp))
  return(paste0("as.",tmp))
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

#' Use a dataframe in a package including structure based documentation
#' 
#' Using the interfacer framework you can document data during development. 
#' This provides the basic documentation framework for a dataset based on a dataframe
#' in the correct format into the right place.
#' 
#' If this is your only use case for `interfacer` then you will not need
#' to import `interfacer` in your package, as none of the generated code will 
#' depend on it.
#'
#' @param df the data frame to use
#' @param name the name of the variable you wish to use (defaults to whatever the function is called with)
#' @param output where to write data documentation code (defaults to `R/data.R`)
#' @param pkg the package (defaults to current)
#'
#' @concept document
#' @return nothing 
#' @export
use_dataframe = function(df, name = deparse(substitute(df)), output = "R/data.R", pkg=".") {
  pkg = devtools::as.package(pkg)
  tmp = list()
  tmp[[name]] = df
  ex = rlang::expr(usethis::use_data(!!as.symbol(name),overwrite = TRUE))
  suppressMessages(with(tmp, eval(ex)))
  .write_to_source(df, name, template = "templates/data.R.template", output = output, pkg=pkg)
}

#' Generate interfacer code for a dataframe
#' 
#' Generating and documenting an `iface` for a given dataframe would be time
#' consuming and annoying if you can't do it automatically. In this case as you
#' interactively develop a package any given dataframe's structure can be
#' explicitly documented and made into a specific contract within the package.
#'
#' @param df the data frame to use
#' @param name the name of the variable you wish to use (defaults to whatever
#'   the function is called with)
#' @param output where to write data documentation code (defaults to `R/data.R`)
#' @param pkg the package (defaults to current)
#' @param use_as_default if this is set to true the current dataframe is saved
#'   as package data and the `interfacer::iface` specification is created
#'   referring to the package copy of the current dataframe as the default
#'   value.
#'   
#' @concept document
#' @return nothing
#' @export
use_iface = function(df, name = deparse(substitute(df)), output = "R/interfaces.R", use_as_default = FALSE, pkg=".") {
  pkg = devtools::as.package(pkg)
  if (use_as_default) {
    tmp = list()
    tmp[[name]] = df
    ex = rlang::expr(usethis::use_data(!!as.symbol(name),overwrite = TRUE))
    suppressMessages(with(tmp, eval(ex)))
  }
  .write_to_source(df, name, template = "templates/interfaces.R.template", output = output, pkg=pkg, default_variable = if(use_as_default) sprintf("%s::%s",pkg$package,name) else NULL)
}


# write a data frame to a source file using the template
.write_to_source = function(df, name, ..., template = "templates/data.R.template", output = "R/data.R", pkg, default_variable = NULL) {
  
  spec = .infer_structure(df, use_as_default = !is.null(default_variable))
  
  if (rlang::is_missing(name)) {
    name = deparse(substitute(df))
  }
  
  # get rid of package declaration for name
  if (stringr::str_detect(name,"::")) name = stringr::str_extract(name,"::(.*)$",1) %>% unlist()
  
  # name = "test"
  
  data_r = fs::path(pkg$path, output)
  
  if (fs::file_exists(data_r)) {
    data_r_lines = readr::read_lines(data_r)
  } else {
    data_r_lines = character()
  }
  
  # clear previous documentation
  start_loc = which(data_r_lines==sprintf("## %s definition ----",name))
  end_loc = which(data_r_lines==sprintf("## %s definition ends",name))
  if (length(start_loc)==1 && length(end_loc) ==1) {
    if (!data_r_lines[start_loc+1] == "## Generated code. remove this line to prevent manual changes being overwritten") {
      stop("A matching code block was found in \n",output, "\nbut it is looks like it has been manually updated.\naborting update.")
    } else {
      data_r_lines = data_r_lines[-1*start_loc:end_loc]
    }
  } else if (length(start_loc)!=0 && length(end_loc) !=0) {
    stop("A partly matching or multiple matching code blocks were found in\n",output, "\nit is in an inconsistent state and will need manual editing.\naborting update.")
  }
  
  tmpl_list = as.list(spec,flatten=TRUE)
  tmpl_list$md = unlist(stringr::str_split(knitr::knit_print(spec),"\\n"))
  tmpl_list$code = .generate_code(spec, default_variable)
  tmpl_list$name = name
  tmpl_list$default_variable = default_variable
  tmpl_list$nrow = nrow(df)
  tmpl_list$ncol = ncol(df)
  
  # add 
  template_content = readr::read_file(system.file(template, package="interfacer"))
  tmp = whisker::whisker.render(template_content, tmpl_list)
  data_r_lines = c(data_r_lines,
                   unlist(stringr::str_split(tmp,"\\n")))
  message("Updating code in: ", data_r)
  readr::write_lines(data_r_lines, data_r)
}


.converter_names = function() {
  e = loadNamespace("interfacer")
  ls(envir = e,pattern = "type") %>% stringr::str_remove("type.")
}

.converters = function() {
  e = loadNamespace("interfacer")
  ls(envir=e,pattern = "type") %>% stringr::str_remove("type.") %>%
    .none(fmt_item="`%s`")
}
