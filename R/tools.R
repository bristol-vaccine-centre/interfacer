
# TODO: convert a dataframe to an interface spec dynamically
# rationale is that there is something you have to conform the output to or 
# input as
# as.iface.dataframe

# TODO: nested list columns and recursion.

#' Create an `iface` interface from an example dataframe
#'
#' Copies the `iface` to the clipboard
#'
#' @param df a prototype dataframe
#'
#' @concept document
#'
#' @return nothing
#' @export
#'
#' @examples
#' if(FALSE) iclip(iris)
iclip = function(df) {
  names = colnames(df)
  types = unname(sapply(df,
    function(x) {
      if (inherits(x,"factor")) {
        return(sprintf("enum(%s)", paste0("`",levels(x),"`",collapse=",")))
      }
      if (is.list(x)) return(sprintf("list(%s)",class(x[[1]])[[1]]))
      tmp = class(x)[[1]]
      if (tmp %in% names(.conv)) return(tmp)
      return(paste0("as.",tmp))
    }
  ))
  docs = sprintf("the %s column",names)
  groups = .none(dplyr::group_vars(df),collapse = " + ",fmt = ".groups = ~ %s",none = ".groups = FALSE")
  tmp = sprintf("interfacer::iface(\n\t%s\n)",
    paste0(
      c(
        sprintf("%s = %s ~ \"%s\"", names, types, docs),
        groups
      ),
      collapse = ",\n\t"
    )
  )
  message(deparse(substitute(df)), " specification copied to clipboard... Ctrl-V to paste.")
  clipr::write_clip(tmp)
}


