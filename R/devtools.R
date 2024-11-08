
#' Create an `iface` specification from an example dataframe
#'
#' When developing with `interfacer` it is useful to be able to base a function
#' input off a prototype that you are for example using as testing. This
#' function generates an `interfacer::iface` specification for the supplied data
#' frame and copies it  to the clipboard so that it can be pasted into the
#' package code you are working on.
#' 
#' If the dataframe contains one or more list columns with nested dataframes
#' the nested dataframes are also defined using a second `iface` specification.
#'
#' @param df a prototype dataframe
#' @param df_name an optional name for the parameter (defaults to `i_<df name>`)
#'
#' @concept document
#'
#' @return nothing, populates clipboard
#' @export
#'
#' @examples
#' if (interactive()) iclip(iris)
iclip = function(df, df_name=deparse(substitute(df))) {
  
  if (!interactive()) stop("`iclip` is only designed to be used interactively.")
  if (!utils::askYesNo(glue::glue("This function will write an interface specification onto the clipboard replacing current contents. Do you want to continue?"))) {
    warning("Operation cancelled.")
    return(invisible(NULL))
  }
  
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

#' Document an interface contract for inserting into `roxygen2`
#'
#' This function is expected to be called within the documentation of a function
#' as inline code in the parameter documentation of the function. It details the
#' expected columns that the input dataframe should possess. This has mostly
#' been superseded by the `@iparam <name> <description>` `roxygen2` tag which does 
#' this automatically, however in some circumstances (particularly multiple 
#' dispatch) you may want to assemble dataframe documentation manually.
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

#' Parser for `@iparam` tags
#' 
#' The `@iparam <name> <description>` tag can be used in `roxygen2` documentation
#' of a function to describe a dataframe parameter. The function must be using
#' `interfacer::iface` to define the input dataframe parameter format. The
#' `@iparam` tag will then generate documentation about the type of dataframe
#' the function is expecting.
#' 
#' @inheritParams roxygen2::roxy_tag_parse
#' @concept document
#' 
#' @importFrom roxygen2 roxy_tag_parse
#' @return a `roxy_tag` object with the `val` field set to the parsed value
#' @export
#' @examples
#' # This provides support to `roxygen2` and only gets executed in the context
#' # of `devtools::document()`. There is no interactive use of this function.
roxy_tag_parse.roxy_tag_iparam <- function(x) {
  roxygen2::tag_two_part(x,"param","description")
}

#' Support for `@iparam` tags
#' 
#' The `@iparam <name> <description>` tag can be used in `roxygen2` documentation
#' of a function to describe a dataframe parameter. The function must be using
#' `interfacer::iface` to define the input dataframe parameter format. The
#' `@iparam` tag will then generate documentation about the type of dataframe
#' the function is expecting.
#' 
#' @inheritParams roxygen2::roxy_tag_rd
#' 
#' @importFrom roxygen2 roxy_tag_rd
#' @concept document
#' @return an `roxygen2::rd_section` (see `roxygen2` documentation)
#' @export
#' 
#' @examples 
#' 
#' # An example function definition:
#' fn_definition <- "
#' #' This is a title
#' #' 
#' #' This is the description.
#' #' 
#' #' @md
#' #' @iparam df the input
#' #' @export
#' f <- function(df = interfacer::iface(
#'   id = integer ~ \"an integer `ID`\",
#'   test = logical ~ \"the test result\"
#' )) {
#'   ivalidate(df)
#' }
#' "
#' 
#' # For this example we manually parse the function specification in `fn_definition`
#' # creating a .Rd block - normally this is done by `roxygen2` which then
#' # writes this to an .Rd file. This function is not intended to be used 
#' # outside of a call to `devtools::document`.
#' 
#' tmp = roxygen2::parse_text(fn_definition)
#' print(tmp)
#' 
roxy_tag_rd.roxy_tag_iparam <- function(x, base_path, env) {
  
  dname = x$val$name
  block = .search_call_stack(.class="roxy_block")
  fn = block$object$value
  icall = formals(fn)[[dname]]
  desc = x$val$description
  
  if (rlang::is_missing(icall) || is.null(icall)) {
    # There is no formal parameter with a default value (or maybe it is given as NULL)
    # we do not try and include it in the documentation
    out = desc
  } else {
  
    # There is a formal parameter with a default value. We try and interpret it
    spec = tryCatch(
      # try evaluate the call as an iface.
      eval(icall,envir = rlang::fn_env(fn)),
      error = function(e) {
        # just return the raw call
        as.character(icall)
      }
    )
    
    if (is.iface(spec)) {
      grps = attr(spec,"groups")
      allow_other = attr(spec,"allow_other")
      default = attr(spec,"default")
      if (!is.null(default)) opt = "A default value is defined."
      else  opt = ""
      
      if (allow_other) {
        if (length(grps)==0) g = "Any grouping allowed."
        else g = sprintf("Minimally grouped by: %s (and other groupings allowed).",paste0(grps,collapse = " + "))
      } else {
        if (length(grps)==0) g = "Ungrouped."
        else g = sprintf("Must be grouped by: %s (exactly).",paste0(grps,collapse = " + "))
      }
      
      out = sprintf("%s

A dataframe containing the following columns: 
\\itemize{
%s
}

%s

%s
",
        desc,
        paste0(glue::glue_data(spec, "\\item {name} ({type}) - {doc}"), collapse = "\n"),
        g,
        opt
      ) %>% trimws()
    } else {
      # spec evaluated to something but not an iface spec. We'll 
      out = sprintf("%s - (defaults to \\code{%s})", desc, deparse(spec))
    }
  }
  names(out) = dname
  roxygen2::rd_section("param", out)
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
#' @return nothing, used for side effects.
#' @export
#' @examples
#' # example code
#' if (interactive()) {
#'   # This is not run as it is designed for interactive use only and will
#'   # write to the userspace after checking that is what the user wants.
#'   use_dataframe(iris) 
#' }
use_dataframe = function(df, name = deparse(substitute(df)), output = "R/data.R", pkg=".") {
  if (!interactive()) stop("This function can only be used interactively.")
  if (!utils::askYesNo(glue::glue("This function will write dataframe documentation into `{output}`. Do you want to continue?"))) {
    warning("Operation cancelled.")
    return(invisible(NULL))
  }
  pkg = devtools::as.package(pkg)
  tmp = list()
  tmp[[name]] = df
  ex = rlang::expr(usethis::use_data(!!as.symbol(name),overwrite = TRUE))
  suppressMessages(with(tmp, eval(ex)))
  tryCatch(
    .write_to_source(df, name, template = "templates/data.R.template", output = output, pkg=pkg),
    error = function(e) {
      warning("An error occurred updating the documentation: ",e$message)
    }
  )
}

#' Generate interfacer code for a dataframe
#' 
#' Generating and documenting an `iface` for a given dataframe would be time
#' consuming and annoying if you could not do it automatically. In this case as
#' you interactively develop a package using a test dataframe, the structure of
#' which can be explicitly documented and made into a specific contract within
#' the package. This supports development using test dataframes as a prototype
#' for function ensuring future user input conforms to the same expectations as
#' the test data.
#'
#' @param df the data frame to use
#' @param name the name of the variable you wish to use (defaults to whatever
#'   the dataframe was called)
#' @param output where within the current package to write data documentation
#'   code (defaults to `R/interfaces.R`)
#' @param pkg the package (defaults to current)
#' @param use_as_default if this is set to true the current dataframe is saved
#'   as package data and the `interfacer::iface` specification is created
#'   referring to the package copy of the current dataframe as the default
#'   value.
#'   
#' @concept document
#' @return nothing, used for side effects.
#' @export
#' @examples
#' # example code
#' if (interactive()) {
#'   # This is not run as it is designed for interactive use only and will
#'   # write to the userspace after checking that is what the user wants.
#'   use_iface(iris) 
#' }
use_iface = function(df, name = deparse(substitute(df)), output = "R/interfaces.R", use_as_default = FALSE, pkg=".") {
  if (!interactive()) stop("This function can only be used interactively.")
  if (!utils::askYesNo(glue::glue("This function will write an interface specification into `{output}`. Do you want to continue?"))) {
    warning("Operation cancelled.")
    return(invisible(NULL))
  }
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
